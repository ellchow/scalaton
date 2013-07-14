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

import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.scalacheck._

import scalaz._
import Scalaz._

import scalaton.util.hashing._
import scalaton.util.hashing128._
import scalaton.aggregate.hashed.hcollection._
import scalaton.aggregate.hashed.sketch._
// import scalaton.aggregate.hashed.mutable.sketch._

class CountMinSketchSpec extends Specification with ScalaCheck{

  "an empty count min sketch" should {

    trait CMS
    def tag[A](x: A) = Tag[A, CMS](x)
    implicit val cmsinstance = countminsketch[String,Bits128,CMS]((5,60),0)

    "return zero for anything" ! prop { (a: String) => lookup(cmsinstance.zero, a) must_== 0L }

    "equal to another empty sketch" in {
      val cmsz = tag((Vector.fill[Long](5, 60)(0L), 0L))

      (cmsinstance.zero === cmsz) must beTrue
    }

    "not equal to any nonempty sketch" ! prop { (a: String) => (cmsinstance.zero === singleton(tag(a), 1L)) must beFalse}
  }

  "a nonempty count min sketch" should {
    trait CMS
    def tag[A](x: A) = Tag[A, CMS](x)


    "should have nonzero estimate for any item that has been updated" ! prop { (as: List[String]) =>
      implicit val cmsinstance = countminsketch[String,Bits128,CMS]((5,60),0)

      val cms = (cmsinstance.zero /: as)((running, a) => update(running, a, 1L))

      as foreach { a => lookup(cms, a) must beGreaterThan(0L) }
    }

    "should estimate within error bounds" in {
      val parameters = for{
        eps <- Seq(0.05, 0.01, 0.001)
        delta <- Seq(0.95, 0.999)
      } yield (eps, delta)

      parameters flatMap { case (eps, delta) =>
        val params = countminsketch.optimalParameters(eps,delta)
        implicit val cmsinstance = countminsketch[String, Bits128, CMS](params, 0)

        for(_ <- (0 to 10)) yield {
          val items = (0 to 5000).view.map( _ => (math.sqrt(SRandom.nextInt(10000)).toInt.toString, (SRandom.nextInt(20) + 1).toLong))

          val trueCounts = items groupBy (_._1) map { case (k, vs) => (k, vs.map(_._2).sum) } toMap

          val cms = (cmsinstance.zero /: items)((running, i) => update(running, i._1, i._2))

          val exceedsMaxError = trueCounts.map{ case (i,c) => (lookup(cms, i) < (1 + eps) * trueCounts.getOrElse(i,0L)) ? 1 | 0 }.sum

          (exceedsMaxError.toDouble / items.size) must beLessThan(delta)
        }
      }
    }
  }

}

