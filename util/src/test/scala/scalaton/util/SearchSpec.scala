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

package scalaton.util

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._

import org.scalacheck._

class SearchSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  import search._

  behavior of "binary search"

  it should "find existing items in sorted seq" in {
    forAll {
      (xs: Set[Int]) => whenever(xs.size > 0) {
        val x = scala.util.Random.shuffle(xs.toVector).head
        val sorted = xs.toVector.sorted

        binarySearch(sorted, x) should be(Right(sorted.indexOf(x)))
      }
    }
  }

  it should "return 0 for empty seq" in {
    forAll{
      (x: Int) => binarySearch(Vector[Int](), x) should be(Left(0))
    }
  }

  it should "identify indices at which to insert missing items" in {
    forAll{
      (xs: Set[Int]) => whenever(xs.size > 0){
        val x = scala.util.Random.shuffle(xs.toVector).head
        val sorted = xs.toVector.sorted
        val i = sorted.indexOf(x)
        val dropped = sorted.take(i) ++ sorted.drop(i + 1)

        binarySearch(dropped, x) should be(Left(i))
      }
    }
  }
}

