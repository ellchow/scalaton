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

package scalaton.collection

import org.scalacheck._
import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._
import scala.util.{ Try, Success, Failure }
import scalaz._, Scalaz._

class JoinSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  import Join._

  behavior of "inner join"

  def checkInnerJoin(xs: Seq[(Int, String)], ys: Seq[(Int, String)], joined: Seq[(Int, (String, String))]) = {
    val joinedKeys = joined.map(_._1)

    val xkeys = xs.map(_._1)
    val ykeys = ys.map(_._1)
    val sharedKeys = xkeys.toSet.intersect(ykeys.toSet)

    val xkcounts = xkeys.map(k => Map(k -> 1)).foldLeft(Map.empty[Int,Int])(_ |+| _)
    val ykcounts = ykeys.map(k => Map(k -> 1)).foldLeft(Map.empty[Int,Int])(_ |+| _)
    val joinedKeyCounts = joinedKeys.map(k => Map(k -> 1)).foldLeft(Map.empty[Int,Int])(_ |+| _)

    joinedKeys.toSet should be(sharedKeys)

    val sharedCounts = for {
      (kx, cx) <- xkcounts
      (ky, cy) <- ykcounts
      if kx == ky
    } yield joinedKeyCounts(kx) should be(cx * cy)
  }

  def checkFullOuterJoin(xs: Seq[(Int, String)], ys: Seq[(Int, String)], joined: Seq[(Int, (Option[String], Option[String]))]) = {
    val joinedKeys = joined.map(_._1)

    val xkeys = xs.map(_._1)
    val ykeys = ys.map(_._1)
    val allKeys = xkeys.toSet ++ ykeys.toSet

    val xkcounts = xkeys.map(k => Map(k -> 1)).foldLeft(Map.empty[Int,Int])(_ |+| _)
    val ykcounts = ykeys.map(k => Map(k -> 1)).foldLeft(Map.empty[Int,Int])(_ |+| _)
    val joinedKeyCounts = joinedKeys.map(k => Map(k -> 1)).foldLeft(Map.empty[Int,Int])(_ |+| _)

    joinedKeys.toSet should be(allKeys)

    val sharedCounts = for {
      (kx, cx) <- xkcounts
      (ky, cy) <- ykcounts
      if (kx == ky)
    } yield joinedKeyCounts(kx) should be(cx * cy)

    val xOnlyCounts = for {
      (kx, cx) <- xkcounts
      if !ykeys.contains(kx)
    } yield joinedKeyCounts(kx) should be(cx)

    val yOnlyCounts = for {
      (ky, cy) <- ykcounts
      if !xkeys.contains(ky)
    } yield joinedKeyCounts(ky) should be(cy)

  }

  it should "for hash-join, return all and only pairs of elements whose key is in both collections" in {
    forAll {
      (xs: Map[Int,String], ys: List[(Int, String)]) => {
        val joined = xs.innerJoin(ys).toList

        checkInnerJoin(xs.toSeq, ys, joined)
      }
    }
  }

  it should "for sorted collection, return all and only pairs of elements whose key is in both collections" in {
    forAll {
      (xs: List[(Int,String)], ys: List[(Int, String)]) => {
        val joined = xs.sorted.innerJoin(ys.sorted).toList

        checkInnerJoin(xs, ys, joined)
      }
    }
  }

  behavior of "full outer join"

  it should "return all pairs of elements with keys in both collections as (Some, Some) and elements with keys in only collection as (Some, None) or (None, Some)" in {
    forAll {
      (xs: List[(Int,String)], ys: List[(Int, String)]) => {
        val joined = xs.sorted.fullOuterJoin(ys.sorted).toList

        checkFullOuterJoin(xs, ys, joined)
      }
    }
  }


}
