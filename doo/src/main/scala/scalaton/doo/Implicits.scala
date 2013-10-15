/*
 Copyright 2013 Elliot Chow

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

package scalaton.doo

import com.github.nscala_time.time.Imports._

import collection.immutable.TreeMap
import collection.mutable

import java.io._
import org.apache.hadoop.conf.{Configuration => HConf}

import scalaton.util._
import scalaton.util.hashing._
import scalaton.util.hashing32._
import scalaton.aggregate.moments._
import scalaton.aggregate._

import com.googlecode.javaewah.{EWAHCompressedBitmap => CompressedBitSet}

import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.core.{Reduction, ScoobiConfiguration}

import com.typesafe.scalalogging.slf4j._

import scalaz.{DList => _, _}
import Scalaz._
import Tree._

trait ImplicitConversions{

  // DLists
  implicit class DListOps[A : Manifest : WireFormat](val dl: DList[A]) extends Logging{

    def limit(n: Int = 0) = sampling.limit(dl, n)

    def parallelFold[B : Manifest : WireFormat](init: B)(f: (B, A) => B) =
      helpers.parallelFold(dl, init)(f)

    def parallelFoldMonoid[B : Manifest : WireFormat : Monoid](f: (B, A) => B) =
      helpers.parallelFoldMonoid(dl)(f)

    def sample(rate: Double, seed: Int = 0) =
      sampling.sample(dl, rate, seed)

    def sampleBy[B : Manifest : WireFormat](f: A => B)(rate: Double, seed: Int = 0)(implicit hashable: Hashable[B,Bits32]) =
      sampling.sampleBy(dl.map(a => (f(a), a)), rate, seed)

    def checkpointToSequenceFile(path: String, overwrite: Boolean = false)(implicit sc: ScoobiConfiguration): DList[A] = {
      if(overwrite && hdfs.exists(path, sc.configuration))
        hdfs.delete(path, true, sc.configuration)

      val withDummyKey = dl.map(x => (0: Byte, x))

      val checkpointed = withDummyKey.toSequenceFile(path, overwrite, checkpoint = true)

      checkpointed.map(_._2)
    }

    def hcountN(filters: List[(String, A => Boolean)]) =
      helpers.hcountN(dl, filters)

    def applyAll[L](functions: List[DList[A] => DList[A]]): DList[A] =
      helpers.applyAll(dl, functions)

    def filterN(filters: List[(String, A => Boolean)], emitCounters: Boolean = false) =
      helpers.filterN(dl, filters, emitCounters)

    def split(filters: List[(String, A => Boolean)], emitCounters: Boolean = false) =
      helpers.split(dl, filters, emitCounters)

  }

  implicit class DList2GroupingAOps[A : Manifest : WireFormat : Grouping, B : Manifest : WireFormat](val dl: DList[(A,B)]){
    def bloomJoin[BR : Manifest : WireFormat](right: DList[(A,BR)], expectedNumKeys: Int)(implicit hashable: Hashable[A,Bits32]) =
      joins.bloomJoin(dl, right, expectedNumKeys)

    def skewedJoin[BR : Manifest : WireFormat](right: DList[(A,BR)], sampleRate: Double, maxPerReducer: Int)(implicit hashable: Hashable[A,Bits32]) =
      joins.skewedJoin(dl, right, sampleRate, maxPerReducer)

    def groupByKeyThenCombine(doFlush: collection.Map[A,B] => Boolean = _ => false)(implicit semigroupB: Semigroup[B]) =
      helpers.groupByKeyThenCombine(dl, doFlush)
  }

  // Wireformats

  implicit val jodaLocalDateWF = AnythingFmt[LocalDate]

  implicit def validationFmt[E : WireFormat, A : WireFormat] = new ValidationWireFormat[E, A]
  class ValidationWireFormat[E, A](implicit wt1: WireFormat[E], wt2: WireFormat[A]) extends WireFormat[Validation[E, A]] {
    def toWire(x: Validation[E, A], out: DataOutput) = x match {
      case Failure(x) => { out.writeBoolean(true); wt1.toWire(x, out) }
      case Success(x) => { out.writeBoolean(false); wt2.toWire(x, out) }
    }

    def fromWire(in: DataInput): Validation[E, A] = {
      val isFailure = in.readBoolean()
      if (isFailure) {
        val x: E = wt1.fromWire(in)
        Failure(x)
      } else {
        val x: A = wt2.fromWire(in)
        Success(x)
      }
    }

    override def toString = "Validation["+wt1+","+wt2+"]"
  }

  implicit def nonEmptyListWF[A : WireFormat] = new NonEmptyListWireFormat[A]
  class NonEmptyListWireFormat[A](implicit wt: WireFormat[A]) extends WireFormat[NonEmptyList[A]]{
    def toWire(x: NonEmptyList[A], out: DataOutput) = {
      wt.toWire(x head, out)
      implicitly[WireFormat[List[A]]].toWire(x tail, out)
    }

    def fromWire(in: DataInput): NonEmptyList[A] = {
      val h = wt.fromWire(in)
      val t = implicitly[WireFormat[List[A]]].fromWire(in)
      NonEmptyList(h, t : _*)
    }

    override def toString = "NonEmptyList["+wt+"]"
  }

  implicit def scalazTreeWF[A : WireFormat] = new WireFormat[Tree[A]]{
    def toWire(t: Tree[A], out: DataOutput) = {
      // write the root label
      implicitly[WireFormat[A]].toWire(t.rootLabel, out)

      val sf = t.subForest.toVector

      // write number of children
      out.writeInt(sf.size)

      // write subtrees
      sf.foreach( st => toWire(st, out) )
    }

    def fromWire(in: DataInput): Tree[A] = {
      // read root label
      val rootLabel = implicitly[WireFormat[A]].fromWire(in)

      // read number of children
      val numChildren = in.readInt()

      val buf = mutable.ArrayBuffer[Tree[A]]()
      val stream = buf.mapResult(_.toStream)

      // read each subtree
      for(i <- 1 to numChildren){
        buf += fromWire(in)
      }

      // instantiate Tree
      node(rootLabel, stream.result())
    }
  }

  implicit val compressedBitSetWF = AnythingFmt[CompressedBitSet]

  implicit val momentsWF = mkCaseWireFormat((n: Long, mean: Double, m2: Double, m3: Double, m4: Double) => Moments(n,mean,m2,m3,m4), Moments.unapply _)

  implicit def histogramDataWF[A, B : WireFormat : HistogramValue : Monoid] = new HistogramDataWireFormat[A, B]

  class HistogramDataWireFormat[A, B : WireFormat : HistogramValue : Monoid] extends WireFormat[HistogramData[A, B]]{
    def toWire(x: HistogramData[A, B], out: DataOutput) = {
      implicitly[WireFormat[TreeMap[Double, B]]].toWire(x.buckets, out)
      out.writeDouble(x.min)
      out.writeDouble(x.max)

      x match {
        case HistogramDataLTE1(_, _, _) => out.writeBoolean(true)
        case HistogramDataN(_, _, _) => out.writeBoolean(false)
      }
    }

    def fromWire(in: DataInput): HistogramData[A, B] = {
      val buckets = implicitly[WireFormat[TreeMap[Double, B]]].fromWire(in)
      val min = in.readDouble()
      val max = in.readDouble()
      val isLte1 = in.readBoolean()

      if(isLte1) HistogramDataLTE1(buckets, min, max) else HistogramDataN(buckets, min, max)
    }
  }


  // Reductions

  implicit def funToReduction[A](f: (A, A) => A) = Reduction(f)

}

object implicits extends ImplicitConversions
