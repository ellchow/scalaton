/*
 Copyright 2014 Elliot Chow

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

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._

import org.scalacheck._

import spire.implicits._
import spire.math._

import scalaz._
import Scalaz._

class DyadicDecompositionSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  behavior of "dyadicDecomposition"

    implicit val genInterval: Gen[(Int, Int)] = for{
      lb <- Gen.choose(1, 10000)
      delta <- Gen.choose(0, Int.MaxValue - 1 - lb)
    } yield (lb, lb + delta )

    implicit val arbitraryInterval = Arbitrary(genInterval)


  it should "generate adjacent, non-overlapping intervals" in {
    forAll{
      (interval: (Int, Int)) => {
        val (lb, ub) = interval
        val xs = dyadicDecomposition(lb, ub)
        val sorted = xs.sortBy(_._1)
        (true /: sorted.map(_._2).zip(sorted.drop(1).map(_._1))){ case (y, (a, b)) => y && (a == b - 1) } should be(true)
      }
    }
  }

  it should "generate intervals of power-of-2 size" in {
    forAll{
      (interval: (Int, Int)) => {
        def isPowerOf2(x: Int) = (x != 0) && ((x & (x - 1)) == 0)

        val (lb, ub) = interval
        val xs = dyadicDecomposition(lb, ub)
        val sizes = xs.map{ case (a, b) => (b - a) + 1 }
        (true /: sizes.map(n => isPowerOf2(n)))(_ && _) should be(true)
      }
    }
  }

  it should "generate intervals where min and max of all intervals is equal to the lower and upper bounds" in {
    forAll{
      (interval: (Int, Int)) => {
        val (lb, ub) = interval
        val xs = dyadicDecomposition(lb, ub)
        val min = xs.map(_._1).min
        val max = xs.map(_._2).max

        min should be(lb)
        max should be(ub)
      }
    }
  }
}
