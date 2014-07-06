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

package scalaton.aggregate.hashed

import scalaton.util._
import scalaton.util.Hash.MurmurHash32Implicits._
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


class StandardBloomFilterSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  behavior of "standard bloom filter"

  it should "not contain anything if empty" in {
    forAll{ (s: String) =>
      StandardBloomFilter.empty[String](5,125,0L).contains(s) should be(false)
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

        val x = StandardBloomFilter.fromData(h,w,s)(items)

        items.map(i => x.contains(i)).exists(_ == false) should be(false)
      }
    }
  }

  it should "be below false-positive rate" in {
    val genSettings = for{
      fpProb <- Gen.oneOf(0.1, 0.05, 0.01, 0.005)
      numItems <- Gen.choose(1, 100)
      s <- Arbitrary.arbitrary[Long]
    } yield (numItems, fpProb, s)

    forAll(genSettings) { (settings: (Int, Double, Long)) =>
      whenever(settings._1 > 0 && settings._1 < 50){
        val (numItems, fpProb, s) = settings
        val (h, w) = BloomFilter.optimalParameters(100, fpProb)

        val fps = (0 until 100).map{ _ => StandardBloomFilter.fromData(h,w,s)(0 until numItems).contains(-1) ? 1.0 | 0.0 }

        (fps.sum / fps.size) should be < (1.05 * fpProb)
      }
    }
  }

  it should "estimate size well for elements less than the intended number of elements" in {
    forAll{ (xs: Set[String]) =>
      whenever(xs.size > 0){
        val (h,w) = BloomFilter.optimalParameters(xs.size * 10, 0.05)
        val err = math.abs(StandardBloomFilter.fromData(h,w,0L)(xs).size.get - xs.size).toDouble / xs.size
        err should be <= (0.1 max (1.0 / xs.size))
      }
    }
  }
}
