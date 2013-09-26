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

package scalaton.aggregate

import collection.immutable.TreeMap

import scalaz._
import Scalaz._

import spire.implicits._

trait HistogramModule{
  import moments._


  abstract class HistogramValue[A : Monoid]{
    def count(a: A): Long
  }

  implicit def simpleHistogramValue = new HistogramValue[Long]{
    def count(a: Long) = a
  }
  implicit def histogramWithTargetValue[Y : Monoid] = new HistogramValue[(Long, Y)]{
    def count(a: (Long, Y)) = a._1
  }


  abstract class HistogramPoint[A, P: Numeric, B : HistogramValue : Monoid]{
    def point(a: A): P

    def pointAsDouble(a: A): Double = implicitly[Numeric[P]].toDouble(point(a))

    def value(a: A): B
  }

  implicit def simpleHistogramPoint[A : Numeric] = new HistogramPoint[A, A, Long]{
    def point(a: A) = a

    def value(a: A) = 1L
  }

  case class HistogramData[B : HistogramValue : Monoid](val buckets: TreeMap[Double, B])

  abstract class Histogram[A, P, B, T](implicit num: Numeric[P], mon: Monoid[B], hv: HistogramValue[B], hp: HistogramPoint[A,P,B]){
    val maxBuckets: Int

    def empty: HistogramData[B] @@ T = Tag(HistogramData[B](TreeMap[Double,B]()))

    def gapSize(x: (Double, Long), y: (Double, Long)): Double = y._1 - x._1

    def insert(h: HistogramData[B] @@ T, a: A): HistogramData[B] @@ T =
      merge(h, Tag(HistogramData[B](TreeMap(hp.pointAsDouble(a) -> hp.value(a)))) )

    def merge(h1: HistogramData[B] @@ T, h2: HistogramData[B] @@ T): HistogramData[B] @@ T = {
      val unmergedBuckets = implicitly[Monoid[Map[Double, B]]].append((h1.buckets : Map[Double, B]), (h2.buckets : Map[Double, B])).asInstanceOf[TreeMap[Double, B]]

      @annotation.tailrec
      def loop(bs: TreeMap[Double, B]): TreeMap[Double, B] = {
        if(bs.size lte maxBuckets)
          bs
        else{
          val (_, (k1, k2)) = ((Double.NegativeInfinity, (0.0, 0.0)) /: bs.zip(bs.drop(1))) {
            case (maxSoFar, ((k1,v1), (k2,v2))) =>
              val w = gapSize((k1, hv.count(v1)),
                              (k2, hv.count(v2)))

              (w gt maxSoFar._1) ? (w, (k1, k2)) | maxSoFar
          }

          val v1 = bs(k1)
          val v2 = bs(k2)

          val k = (k1 * hv.count(v1) + k2 * hv.count(v2)) / (hv.count(v1) + hv.count(v2))
          val v = mon.append(v1,v2)

          loop(bs - k1 - k2 + (k -> v))
        }
      }

      Tag(HistogramData[B](loop(unmergedBuckets)))
    }
  }

  def simpleHistogram[A, T](n: Int)(implicit num: Numeric[A], hp: HistogramPoint[A,A,Long]) = new Histogram[A, A, Long, T]{ val maxBuckets = n }

}

object hist extends HistogramModule
