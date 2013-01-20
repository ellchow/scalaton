package scalaton.stats.singlepass

import org.specs2.mutable._

import scalaz.{BloomFilter => _, _}
import Scalaz._

import scalaton.util.hashable._
import scalaton.stats.singlepass.bloomfilter._

class BloomFilterSpec extends Specification{
  "an empty bloom filter" should {
    implicit val bfmon = bfmInstance[String,(Long,Long)](5,625)
    val bfz: BloomFilter[String,(Long,Long)]= BFZero[String,(Long,Long)](5, 625)
    val random = new util.Random(0)

    "not contain anything" in {
      0 to 1000 foreach { i =>
        bfz contains (random nextDouble() toString) must beFalse
      }
    }

    "be empty when added with another empty bloom filter" in {

      ((bfz |+| bfz) === bfz) must beTrue
    }

    "contain the item after adding it" in {

      (bfz + "a" contains "a") must beTrue
    }

    "be equal to the other bloom filter after add another" in {
      val bf = bfz + "a"

      ((bfz |+| bf) === bf) must beTrue
    }

  }


    // "should be below false-positive rate with high confidence" in {

    // }

}
