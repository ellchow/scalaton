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

import scalaton.util._
import scalaton.util.hashing._

import scalaton.aggregate.hashed.hcollection._
import scalaton.aggregate.hashed.mutable.bloomfilter._
import com.googlecode.javaewah.{EWAHCompressedBitmap => CompressedBitSet}
import scalaton.aggregate.hashed.mutable.sketch._

import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.core.Reduction

import scalaz.{DList => _, _}
import Scalaz._

import implicits._

trait JoinFunctions{
  def bloomJoin[A : Manifest : WireFormat : Grouping, BL : Manifest : WireFormat, BR : Manifest : WireFormat](
    left: DList[(A,BL)], right: DList[(A,BR)], expectedNumKeys: Int)(
    implicit hashableA: Hashable[A, Bits32]) = {

    trait SBF
    implicit val sbfinst = sbf.sparse[A, Bits32, SBF](sbf.optimalParameters(expectedNumKeys, 0.1))
    implicit val cbsWF = AnythingFmt[CompressedBitSet @@ SBF]

    val leftKeys = helpers.parallelFoldMonoid[A, CompressedBitSet @@ SBF](left map ( _._1 ))((acc, x) => insert(acc, x))
      .reduce(Reduction(_ |+| _))

    val rightFiltered = leftKeys.join(right).mapFlatten{ case (ks, (a, br)) =>
                                                         contains(ks, a) ? (a, br).some | none[(A,BR)] }

    left join rightFiltered
  }

  def skewedJoin[A : Manifest : WireFormat : Grouping, BL : Manifest : WireFormat, BR : Manifest : WireFormat](
    left: DList[(A,BL)], right: DList[(A,BR)], sampleRate: Double = 0.01, maxPerReducer: Int = 100000)(
    implicit hashableA: Hashable[A, Bits32]) = {

    implicit val aiGrouping = new Grouping[(A, Int)] {
      override def sortCompare(first: (A, Int), second: (A, Int)) =
        implicitly[Grouping[A]].sortCompare(first._1, second._1) |+| (first._2 ?|? second._2)

      override def groupCompare(first: (A, Int), second: (A, Int)) = sortCompare(first, second)
    }

    def reps(x: Long): Int = ((x / maxPerReducer) max 1).toInt min 100

    trait CMS
    implicit val cmsinst = countminsketch[A, Bits32, CMS](countminsketch.optimalParameters(0.05, 0.05))
    implicit val tblWF = AnythingFmt[SketchTable[Long] @@ CMS]

    val weight = (1 / sampleRate) toLong

    val rightKeysSample = right.map(_._1) sample sampleRate

    val rightDist = helpers.parallelFoldMonoid[A, SketchTable[Long] @@ CMS](rightKeysSample)((acc, x) => update(acc, x, weight))
      .reduce(Reduction(_ |+| _))

    val rightScattered = rightDist.join(right) map { case (dist, (a, br)) =>
                                                     val n = reps(lookup(dist, a))

                                                     ((a, util.Random.nextInt(n)), br)
                                                }

    val leftSprayed = rightDist.join(left) mapFlatten { case (dist, (a, bl)) =>
                                                            val n = reps(lookup(dist, a))

                                                            (0 until n) map (i => ((a, i), bl))
                                                          }

    leftSprayed.join(rightScattered) map { case ((a, i), v) => (a, v)}
  }

}

object joins extends JoinFunctions
