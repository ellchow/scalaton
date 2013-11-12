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

package scalaton.aggregate.hashed

import scala.util.{Random => SRandom}

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._

import org.scalacheck._

import scalaz._
import Scalaz._

import scalaton.util.tag._
import scalaton.util.hashing128._
import scalaton.aggregate.hashed.hcollection._
import scalaton.aggregate.hashed.sketch._

class CountMinSketchSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  trait CMS


  behavior of "CountMinSketch"

  it should "return zero for everything when empty" in {
    forAll{
      (s: String) => {
        implicit val cmsMonoid = countminsketch[String,Bits128,CMS]((5,60),0)

        lookup(cmsMonoid.zero, s) should be(0L)
      }
    }
  }

  it should "have nonzero estimate for any item that has been updated" in {
    forAll{
      (s: String, n: Long) => whenever(n > 0){
        implicit val cmsMonoid = countminsketch[String,Bits128,CMS]((5,60),0)

        val x = singleton(s.tag[CMS], n)

        lookup(x, s) should be > 0L
      }
    }
  }

  it should "estimate within error bounds" in {
    implicit val genSettings: Gen[(Double, Double)] = for{
      eps <- Gen.oneOf(0.05, 0.01, 0.001)
      delta <- Gen.oneOf(0.95, 0.999)
    } yield (eps, delta)

    implicit val arbitrarySettings = Arbitrary(genSettings)

    implicit val genItems: Gen[Seq[(String, Long)]] = for{
      n <- Gen.choose(1, 10000)
    } yield {
      (1 to n).toSeq.map( _ => (math.sqrt(SRandom.nextInt(10000)).toInt.toString,
                                (SRandom.nextInt(20) + 1).toLong))
    }

    implicit val arbitraryItems = Arbitrary(genItems)

    forAll{
      (settings: (Double,Double), items: Seq[(String, Long)]) => {
        val (eps, delta) = settings
        val params = countminsketch.optimalParameters(settings._1, settings._2)
        implicit val cmsMonoid = countminsketch[String, Bits128, CMS](params, 0)

        val trueCounts = items.groupBy(_._1).map{ case (k, vs) => (k, vs.map(_._2).sum) }.toMap

        val cms = (cmsMonoid.zero /: items)((running, i) => update(running, i._1, i._2))

        val exceedsMaxError = trueCounts.map{ case (i,c) => (lookup(cms, i) < (1 + eps) * trueCounts.getOrElse(i,0L)) ? 1 | 0 }.sum

        (exceedsMaxError.toDouble / items.size) should be < delta
      }
    }
  }


}

