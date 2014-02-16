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

  it should "return elements with keys in both collections" in {
    val joined1 = Seq(1 -> 2, 2 -> 2, 2 -> 3, 3 -> 4).innerJoin(Seq(1 -> 5, 2 -> 6, 2 -> 7, 4 -> 8)).toList.sorted
    joined1 should be(List((1, (2,5)), (2,(2,6)), (2,(2,7)), (2,(3,6)), (2,(3,7))))

    val joined2 = Map(1 -> 2, 2 -> 2, 3 -> 3).innerJoin(Seq(1 -> 4, 2 -> 5, 2 -> 6, 4 -> 7)).toList.sorted
    joined2 should be(List((1, (2,4)), (2,(2,5)), (2,(2,6))))
  }

  it should """for hash-join, keys in joined should be a multiset containing all keys in L \/ R; for each k in L /\ R, count should be |{k} /\ L| x |{k} /\ R|""" in {
    forAll {
      (xs: Map[Int,String], ys: List[(Int, String)]) => {
        val joined = xs.innerJoin(ys).toList

        checkInnerJoin(xs.toSeq, ys, joined)
      }
    }
  }

  it should """for sorted-join, keys in joined should be a multiset containing all keys in L \/ R; for each k in L /\ R, count should be |{k} /\ L| x |{k} /\ R|""" in {
    forAll {
      (xs: List[(Int,String)], ys: List[(Int, String)]) => {
        val joined = xs.sorted.innerJoin(ys.sorted).toList

        checkInnerJoin(xs, ys, joined)
      }
    }
  }

  behavior of "full outer join"

  it should "return all pairs of elements with keys in both collections as (Some, Some) and elements with keys in only one collection as (Some, None) or (None, Some)" in {
    val joined1 = Seq(1 -> 2, 2 -> 2, 2 -> 3, 3 -> 4).fullOuterJoin(Seq(1 -> 5, 2 -> 6, 2 -> 7, 4 -> 8)).toList.sorted
    joined1 should be(List((1,(Some(2),Some(5))), (2,(Some(2),Some(6))), (2,(Some(2),Some(7))), (2,(Some(3),Some(6))), (2,(Some(3),Some(7))), (3,(Some(4),None)), (4,(None,Some(8)))))
  }

  it should """keys in joined should be a multiset containing all keys in L \/ R; for each k in L /\ R, count should be |{k} /\ L| x |{k} /\ R|; for each k in L \ R or R \ L, count should be 1""" in {
    forAll {
      (xs: List[(Int,String)], ys: List[(Int, String)]) => {
        val joined = xs.sorted.fullOuterJoin(ys.sorted).toList

        checkFullOuterJoin(xs, ys, joined)
      }
    }
  }

  behavior of "sorted join"

  it should "fail if collections are not sorted" in {
    intercept[IllegalArgumentException] { Seq((1,1),(3,3),(2,2)).coGroup(Seq((1,1),(3,3),(5,5))).toList }
    intercept[IllegalArgumentException] { Seq((1,1),(3,3),(5,5)).coGroup(Seq((1,1),(3,3),(2,2))).toList }
  }

}
