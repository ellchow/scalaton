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


class StreamSummarySpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  import freqitems._
  import org.apache.commons.math3.distribution._

  behavior of "stream summary"

  it should "update current element count" in {
    val x = StreamSummary.fromData(10, List(1,2,1,1,2,2,2,2))

    x.get(1).get.count should be(3)

    x.get(2).get.count should be(5)
  }

  it should "keep always elements in descending order" in {
    forAll{
      (is: List[Int]) => {
        val x = StreamSummary.fromData(10, is)
        val counts = x.elements.map(_.count)

        counts.zip(counts.drop(1)).map{ case (a,b) => if(a >= b) 0 else 1 }.sum should be(0)
      }
    }
  }

  it should "keep up to specified capacity" in {
    forAll{
      (is: List[Int]) => {
        val x = StreamSummary.fromData(10, is)
        val counts = x.elements.map(_.count)
        x.elements.size should be <= 10
      }
    }
  }

  it should "drop the element with smallest count when inserting into full summary" in {
    val x = StreamSummary.fromData(2, List(1,2,1,1,3))

    x.get(1).get.count should be(3)
    x.get(2) should be(None)
    x.get(3).get.count should be(2)
    x.get(3).get.error should be(1)

    val x1 = x.insert(4)
    x.get(3) should be(None)
    x.get(4).get.count should be(3)
    x.get(4).get.error should be(2)
  }

  it should "approximately maintain top k elements by frequency" in {
    val r = new org.apache.commons.math3.distribution.ExponentialDistribution(Int.MaxValue)
    val as = (0 to 1000000).map{ _ =>
      val x = math.pow(r.sample,0.2).toInt
      x + (if(x > 70) scala.util.Random.nextInt(x * 1000000) else 0)
    }

    val truth = as.groupBy(identity).map{ case (k, vs) => (k, vs.size) }.toSeq.sortBy(-_._2)
    val t = truth.map(_._1).take(20).toSet
    val x = StreamSummary.fromData(50, as).top(20).map(_.key).toSet

    x.intersect(t).size should be >= 17
  }

}
