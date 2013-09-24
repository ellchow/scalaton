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

import org.specs2.mutable._
import scalaz._
import Scalaz._

class SearchSpec extends Specification {
  "binary search" should {
    import search._

    "find existing items in sorted indexed seq" in {
      for(_ <- 0 until 1000) yield {
        val n = util.Random.nextInt(1000) + 1

        val nums = util.Random.shuffle(0 to 1500).take(n)

        val sorted = nums.sorted

        binarySearch(sorted, nums.head) mustEqual Right(sorted.indexOf(nums.head))
      }
    }

    "find index at which to insert missing items" in {
      for(_ <- 0 until 1000) yield {
        val n = util.Random.nextInt(1000) + 1

        val nums = util.Random.shuffle(0 to 1500).take(n)

        val sorted = nums.sorted

        val i = util.Random.nextInt(sorted.size)

        val dropped = sorted.take(i) ++ sorted.drop(i + 1)

        binarySearch(dropped, sorted(i)) mustEqual Left(i)
      }
    }

  }
}
