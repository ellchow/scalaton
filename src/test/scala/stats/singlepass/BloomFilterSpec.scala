package scalaton.stats.singlepass

import org.specs2.mutable._
import scalaz._
import Scalaz._

import scalaton.util.hashable._


import scalaton.stats.singlepass.bloomfilter._

class BloomFilterSpec extends Specification {
  "an empty bloom filter" should {

    "not contain anything" in {

    }

    "be empty when added with another empty bloom filter" in {
      val numHashes = 3
      val width = 40
      // BFZero[String,Long](numHashes, width)
    }

    "contain the item after adding it" in {

    }

    "be equal to the other bloom filter after adding another" in {

    }

    "be equal to the other bloom filter after add another" in {

    }

  }

}
