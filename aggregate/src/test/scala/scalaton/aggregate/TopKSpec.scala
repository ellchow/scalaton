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

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._

import org.scalacheck._

import scalaz._
import Scalaz._

class TopKSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  behavior of "top k algorithm"

  it should "keep top k items" in {
    forAll{
      (xs: Set[Int], k: Int) => whenever(k > 0){
        trait TK
        implicit val tk = topk[Int, Int, TK](k)

        val items = xs.zip(scala.util.Random.shuffle(1 to xs.size))

        topk.fromData(items).toList should be(items.toSeq.sortBy(_._2).takeRight(k).toList)
      }
    }
  }

  it should "merge and keep top k" in {
    forAll{
      (xs: Set[Int], k: Int, ii: Int) => whenever(xs.nonEmpty && k > 0 && ii > 0){
        trait TK
        implicit val tk = topk[Int, Int, TK](k)

        val items = xs.zip(scala.util.Random.shuffle(1 to xs.size))
        val i = ii % xs.size

        val actual = (topk.fromData(items.take(i)) |+| topk.fromData(items.drop(i))).toList

        val expected = items.toList.sortBy(_._2).takeRight(k).toList

        actual should be(expected)
      }
    }
  }

}
