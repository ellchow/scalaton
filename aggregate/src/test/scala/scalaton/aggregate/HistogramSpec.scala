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

import collection.immutable.TreeMap

import org.specs2.mutable._

import scalaton.aggregate.moments._

import scalaz._
import Scalaz._

class HistogramSpec extends Specification{
  import hist._

  def relativeError(x: Double, correct: Double) = math.abs((x - correct) / correct)

  "Histogram object" should {
    "maintain counts for exact bucket center matches" in {
      trait Hst
      val h = simpleHistogram[Double, Hst](5)

      h.insert(h.empty, 1.0).buckets mustEqual TreeMap(1.0 -> 1L)

      h.insert(h.insertN(h.empty, 1.0, 2), 2.0).buckets mustEqual TreeMap(1.0 -> 2L, 2.0 -> 1L)
    }

    "keep a up to a specified maximum number of buckets" in {
      trait Hst
      val h = simpleHistogram[Double, Hst](3)

      var x = h.empty
      for(i <- 1 to 10)
      yield{
        x = h.insert(x, i)

        x.buckets.size must beLessThan(4)
      }


      for(_ <- 1 to 50)
      yield{
        val x = (0 until (util.Random.nextInt(10) + 1)).foldLeft(h.empty){ case (hh, i) => h.insert(hh, i) }
        val y = (0 until (util.Random.nextInt(10) + 1)).foldLeft(h.empty){ case (hh, i) => h.insert(hh, i) }

        h.merge(x,y).buckets.size must beLessThan(4)
      }
    }

    "choose to merge with the closest bucket" in {
      trait Hst
      val h = simpleHistogram[Double, Hst](2)


      for(_ <- 1 to 100)
      yield{
        val a = 0.0
        val an = util.Random.nextInt(10) + 1
        val b = 1.0
        val bn = util.Random.nextInt(10) + 1

        val c = util.Random.nextDouble

        val x = h.insert(h.insertN(h.insertN(h.empty, a, an), b, bn), c)


        val ((d, dn), (e, en)) = (c lte 0.5) ? ((a, an), (b, bn)) | ((b, bn), (a, an))

        x.buckets mustEqual TreeMap(((d * dn + c) / (dn + 1)) -> (dn + 1), e -> en)
      }
    }



  }
}
