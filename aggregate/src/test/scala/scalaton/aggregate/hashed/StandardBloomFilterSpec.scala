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

import scala.collection.BitSet
import scala.collection.mutable.{BitSet => MBitSet}
import com.googlecode.javaewah.{EWAHCompressedBitmap => CompressedBitSet}

import scala.util.{Random => SRandom}

import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.scalacheck._

import scalaz._
import Scalaz._

import scalaton.util.hashing._
import scalaton.util.hashing128._
import scalaton.aggregate.hashed.hcollection._
import scalaton.aggregate.hashed.bloomfilter.sbf
import scalaton.aggregate.hashed.mutable.bloomfilter.{sbf => msbf}

class StandardBloomFilterSpec extends Specification with ScalaCheck{
  trait DSBF
  trait SSBF

  def tagDense[A](a: A) = Tag[A, DSBF](a)
  def tagSparse[A](a: A) = Tag[A, SSBF](a)

  "an empty bloom filter" should {

    implicit val sbfinstance = sbf[String,Bits128,DSBF]((5,625), 0L)

    "not contain anything" ! prop { (a: String) => contains(sbfinstance.zero, a) must beFalse }

    "contain the item after adding it" in {
      contains(singleton(tagDense("a")), "a") must beTrue
    }

    "not equal to a nonempty bloom filter" ! prop { (a: String) => (insert(sbfinstance.zero, a) === sbfinstance.zero) must beFalse}
  }

  "a nonempty bloom filter" should {

    def testTruePositives[D](sbfinst: StandardBloomFilterT[String,Bits128,D]) = {
      implicit val sbfinstance = sbfinst

      val items = 0 to 10 map { _ => SRandom nextDouble() toString }
      val bf = items.foldLeft(sbfinstance.zero)((acc,x) => insert(acc,x))

      items foreach { i => contains(bf, i) must beTrue }
    }

    def testFPProb[D](sbfinst: StandardBloomFilterT[(String,String),Bits128,D],
                      numItems: Int, fpProb: Double) = {
      implicit val sbfinstance = sbfinst

      val fps = (0 until 5000).view map { _ =>
        val items = (0 until numItems).view map { _ => (SRandom nextDouble() toString,
                                                        SRandom nextDouble() toString) }
        val test = (SRandom nextDouble() toString, SRandom nextDouble() toString)

        val bf = items.foldLeft(sbfinstance.zero)((acc,x) => insert(acc, x))
        if(contains(bf, test)) 1.0 else 0.0
      }
      val observed = fps.sum / fps.size

      observed must beLessThan(1.5 * fpProb)
    }

    def testCardinalityEstimate[D](sbfinst: StandardBloomFilterT[String,Bits128,D]) = {
      implicit val sbfinstance = sbfinst
      var bf = sbfinstance.zero

      for(i <- 1 to 100){
        bf = insert(bf, scala.util.Random.nextDouble.toString)

        math.abs(cardinality(bf) - i) must beLessThan(math.max(math.round(1.05 * i), 1).toLong)
      }
    }

    "should contain all true positives" in {
      val params = sbf.optimalParameters(100, 0.05)

      testTruePositives(sbf[String, Bits128, DSBF](params, 0L))

      testTruePositives(msbf.sparse[String, Bits128, SSBF](params, 0L))
    }

    "should be below false-positive rate with high confidence" in {
      Seq(0.1, 0.05, 0.01, 0.005) foreach{ fpProb =>
        val numItems = 20
        val params = sbf.optimalParameters(numItems, fpProb)

        testFPProb(sbf[(String, String), Bits128, DSBF](params, 0L), numItems, fpProb)

        testFPProb(msbf.sparse[(String, String), Bits128, SSBF](params, 0L), numItems, fpProb)
      }
    }

    "should estimate size well for elements less than the intended number of elements" in {
      testCardinalityEstimate(sbf[String,Bits128,DSBF](sbf.optimalParameters(100,0.05),0))

      testCardinalityEstimate(msbf.sparse[String,Bits128,SSBF](sbf.optimalParameters(100,0.05),0))
    }

    "should should return cardinality of -1 if all bloom filter is full" in {
      implicit val sbfinstance = sbf[String,Bits128,DSBF](sbf.optimalParameters(10,0.05),0)

      val bf = tagDense(BitSet((0 until sbfinstance.width) : _*))

      (cardinality(bf) === -1L) must beTrue
    }

  }

}
