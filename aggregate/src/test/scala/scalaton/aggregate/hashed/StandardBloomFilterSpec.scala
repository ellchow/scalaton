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

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._

import org.scalacheck._

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.tag._
import scalaton.util.hashing128._
import scalaton.aggregate.hashed.hcollection._
import scalaton.aggregate.hashed.bloomfilter.sbf
import scalaton.aggregate.hashed.mutable.bloomfilter.{sbf => msbf}

class StandardBloomFilterSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  trait DSBF
  trait SSBF

  behavior of "standard bloom filter"


  it should "not contain anything if empty" in {
    implicit val sbfinstance = sbf[String,Bits128,DSBF]((5,625), 0L)

    forAll{
      (s: String) => contains(sbfinstance.zero, s) should be(false)
    }
  }

  it should "contain the item after adding it" in {
    implicit val sbfinstance = sbf[String,Bits128,DSBF]((5,625), 0L)

    forAll{
      (s: String) => contains(singleton("a".tag[DSBF]), "a") should be(true)
    }
  }

  val genParams: Gen[(Int, Int, Long, List[String])] = for{
    h <- Gen.choose(1,10)
    w <- Gen.choose(1, 1000)
    s <- Arbitrary.arbitrary[Long]
    xs <- Arbitrary.arbitrary[List[String]]
  } yield (h, w, s, xs)

  it should "contain all true positives" in {

    forAll(genParams){
      (inp: (Int,Int,Long,List[String])) => whenever(inp._4.size < 1000){
        val (h, w, s, items) = inp

        def testTruePositives[D](sbfinst: StandardBloomFilterT[String,Bits128,D]) = {
          implicit val sbfinstance = sbfinst
          val bf = items.foldLeft(sbfinstance.zero)((acc,x) => insert(acc,x))

          items.map(i => contains(bf, i)).exists(_ == false) should be(false)
        }

        testTruePositives(sbf[String, Bits128, DSBF]((h, w), s))

        testTruePositives(msbf.sparse[String, Bits128, SSBF]((h, w), s))
      }
    }
  }

  it should "be below false-positive rate" in {
    val genSettings = for{
      fpProb <- Gen.oneOf(0.1, 0.05, 0.01, 0.005)
      numItems <- Gen.choose(1, 100)
      s <- Arbitrary.arbitrary[Long]
    } yield (numItems, fpProb, s)

    forAll(genSettings) {
      (settings: (Int, Double, Long)) => whenever(settings._1 > 0){
        val (numItems, fpProb, s) = settings
        val params = sbf.optimalParameters(100, fpProb)

        def testFPProb[D](sbfinst: StandardBloomFilterT[Int,Bits128,D]) = {
          implicit val sbfinstance = sbfinst

          val fps = (0 until 100).map{ _ =>
            val bf = (0 until numItems).foldLeft(sbfinstance.zero)((acc,x) => insert(acc, x))

            if(contains(bf, -1)) 1.0 else 0.0
          }

          (fps.sum / fps.size) should be < (1.05 * fpProb)
        }

        testFPProb(sbf[Int, Bits128, DSBF](params, 0L))
        testFPProb(msbf.sparse[Int, Bits128, SSBF](params, 0L))
      }
    }
  }

  it should "estimate size well for elements less than the intended number of elements" in {
      forAll{
        (xs: Set[String]) => whenever(xs.size > 0){
          val params = sbf.optimalParameters(xs.size * 10, 0.05)
          implicit val sbfinst = sbf[String,Bits128,DSBF](params, 0)

          val bf = xs.foldLeft(sbfinst.zero)((acc,x) => insert(acc, x))

          (math.abs(cardinality(bf) - xs.size).toDouble / xs.size) should be <= (0.1 max (1.0 / xs.size))
        }
      }
    }

  it should "return cardinality of -1 if all bloom filter is full" in {
    implicit val sbfinstance = sbf[String,Bits128,DSBF](sbf.optimalParameters(10,0.05),0)

    val bf = BitSet((0 until sbfinstance.width) : _*).tag[DSBF]

    cardinality(bf) should be (-1L)
  }
}
