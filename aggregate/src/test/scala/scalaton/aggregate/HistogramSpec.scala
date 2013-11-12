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

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._

import org.scalacheck._

import spire.implicits._
import spire.math._

import scalaz._
import Scalaz._


class HistogramSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  import hstgm._
  import org.apache.commons.math3.distribution._

  behavior of "histogram"

  it should "maintain counts for exact bucket center matches" in {
    trait Hst
    implicit val h = simpleHistogram[Double, Hst](5)

    h.insert(h.empty, 1.0).buckets should be(TreeMap(1.0 -> 1L))

    h.insert(h.insertN(h.empty, 1.0, 2), 2.0).buckets should be(TreeMap(1.0 -> 2L, 2.0 -> 1L))
  }

  it should "keep a up to a specified maximum number of buckets" in {
    forAll{
      (b: Int, xs: List[Int]) => whenever(b > 0){
        trait Hst
        implicit val h = simpleHistogram[Int, Hst](b)

        xs.foldLeft(h.empty)((accum, x) => h.insert(accum, x)).buckets.size should be <= b
      }
    }
  }

  it should "choose to merge with the closest bucket" in {
    trait Hst
    implicit val h = simpleHistogram[Double, Hst](2)

    val a = 0.0
    val an = scala.util.Random.nextInt(10) + 1
    val b = 1.0
    val bn = scala.util.Random.nextInt(10) + 1

    val c = scala.util.Random.nextDouble

    val x = h.insert(h.insertN(h.insertN(h.empty, a, an), b, bn), c)

    val ((d, dn), (e, en)) = (c lte 0.5) ? ((a, an), (b, bn)) | ((b, bn), (a, an))

    x.buckets should be(TreeMap(((d * dn + c) / (dn + 1)) -> (dn + 1), e -> en))
  }

  it should "maintain min and max bounds" in {
    forAll{
      (b: Int, itemsX: List[Double], itemsY: List[Double]) => whenever(b > 1 && (itemsX.nonEmpty || itemsY.nonEmpty)){
        trait Hst
        implicit val h = simpleHistogram[Double, Hst](b)

        val x = h.insert(h.empty, itemsX : _*)
        val y = h.insert(h.empty, itemsY : _*)

        val xy = h.merge(x, y)
        val itemsXY = itemsX ++ itemsY

        (xy.min, xy.max) should be((itemsXY.min,itemsXY.max))
      }
    }
  }

  it should "compute approximate quantiles" in {
    trait Hst
    implicit val h = simpleHistogram[Double, Hst](256)

    val ds = Vector(new ExponentialDistribution(scala.util.Random.nextDouble * scala.util.Random.nextInt(10) + 1),
                    new NormalDistribution(scala.util.Random.nextInt(100), 1),
                    new UniformRealDistribution(scala.util.Random.nextDouble * scala.util.Random.nextInt(10), scala.util.Random.nextDouble * scala.util.Random.nextInt(100) + 100)
                  )

    def nextDistribution = ds(scala.util.Random.nextInt(ds.size))

    val err = for(_ <- 1 to 200)
              yield{
      val r = nextDistribution

      val xs = (0 to 2000) map (_ => r.sample)

      val y = insert(h.empty, xs : _*)

      val q = scala.util.Random.nextDouble

      math.abs(q - r.cumulativeProbability(quantile(y, q).get))
    }

    (err.sum / err.size) should be < 0.02

    err.sorted.drop((err.size * 0.85).toInt).head should be < 0.02
  }
}

