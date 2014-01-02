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

package scalaton.collection.immutable

import org.scalacheck._
import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._
import scala.collection.immutable.Queue
import scala.util.{ Try, Success, Failure }

class TrieSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  behavior of "trie"

  it should "store/retrieve key-value pairs like a Map" in {
    sealed trait MapOp[A,+B]
    case class Insert[A,+B](key: A, value: B) extends MapOp[A,B]
    case class Remove[A](key: A) extends MapOp[A,Nothing]
    case class Get[A](key: A) extends MapOp[A,Nothing]

    lazy val genMapOp: Gen[MapOp[List[Int],Int]] = for {
      i <- Gen.oneOf(1,2,3)
      k <- Arbitrary.arbitrary[List[Int]]
      v <- Arbitrary.arbitrary[Int]
      op = i match {
        case 1 => Insert(k,v)
        case 2 => Remove(k)
        case 3 => Get(k)
      }
    } yield op


    implicit val arbitraryMapOp = Arbitrary(genMapOp)

    forAll {
      (mapOps: List[MapOp[List[Int],Int]]) => {
        var m = Map.empty[List[Int],Int]
        var t = Trie.empty[Int,Int]

        val actual = mapOps.map{ op =>
          op match {
            case Get(k) =>
              val mv = m.get(k)
              val tv = t.get(k)

              if(mv == tv) 0 else 1

            case Insert(k, v) =>
              m = m + (k -> v)
              t = t + (k -> v)

              // if(m.toSet == t.toSet) 0 else 1
              0

            case Remove(k) =>
              m = m - k
              t = t - k

              // if(m.toSet == t.toSet) 0 else 1
              0
          }
        }

        (actual.sum + (if (m == t) 0 else 1)) should be(0)
      }
    }
  }

  it should "allow prefix retrieval and delete" in {
    import scalaz._
    import Scalaz._

    forAll {
      (inputKeys: List[List[Int]]) => whenever(inputKeys.size < 200){
        val inp = inputKeys.zipWithIndex.map{ case (i, idx) => (i.map(ii => math.abs(ii) % 50), idx) }.toMap

        val t = inp.foldLeft(Trie.empty[Int,Int]){ case (tt, (k,v)) => tt + (k -> v) }

        val m = inp.foldLeft(Map.empty[List[Int], Set[Int]]){ case (mm, (k,v)) =>
          val prefixes = (for (i <- 1 to k.size) yield (k.take(i), Set(v))).toMap
          mm |+| prefixes
        }

        val actualR = for ((k, vs) <- m) yield if (t.withPrefix(k).values.toSet == vs) 0 else 1

        actualR.sum should be(0)

        val actualD = for ((k, vs) <- scala.util.Random.shuffle(m.toSeq).take(20)) yield {
          if (t.removePrefix(k).values.toSet.intersect(vs).isEmpty) 0 else 1
        }

        actualD.sum should be(0)
      }
    }
  }

}
