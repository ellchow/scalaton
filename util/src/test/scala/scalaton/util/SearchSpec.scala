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

import org.specs2._

import org.scalacheck._

class SearchSpec extends Specification with ScalaCheck {
  import search._

  def is = {
    "binary search" ^ p^
    "finds existing items in sorted seq" ! prop { (xs: Set[Int]) => (xs.size > 0) ==> {
      val x = scala.util.Random.shuffle(xs.toVector).head
      val sorted = xs.toVector.sorted

      binarySearch(sorted, x) must_== Right(sorted.indexOf(x))
    }} ^ p^
    "identifies the indes at which to insert missing items" ! prop { (xs: Set[Int], i: Int) => (i >= 0 && i < xs.size) ==> {
      val sorted = xs.toVector.sorted
      val x = sorted(i)
      val dropped = sorted.take(i) ++ sorted.drop(i + 1)

      binarySearch(dropped, x) must_== Left(i)
    }}
  }

}
