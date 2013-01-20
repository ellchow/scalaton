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

    "not contain anything" in {
      util.Random.setSeed(0)

      0 to 1000 foreach { i =>
        bfz contains (util.Random nextDouble() toString) must beFalse
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
    /*
    "is only equal to empty bloom filters with same parameters and without any bits set" in {
      implicit val bfmon = bfmInstance[String,(Long,Long)](1,2,3)

      val bfz1: BloomFilter[String,(Long,Long)] =
        BFZero(1, 2, 3)

      val bfz2: BloomFilter[String,(Long,Long)] =
        BFZero(0, 2, 3)

      val bfz3: BloomFilter[String,(Long,Long)] =
        BFZero(1, 0, 3)

      val bfz4: BloomFilter[String,(Long,Long)] =
        BFZero(1, 2, 0)

      val bf1: BloomFilter[String,(Long,Long)] =
        BFInstance[String,(Long,Long)](1,2,s=3)

      val bf2: BloomFilter[String,(Long,Long)] =
        BFInstance[String,(Long,Long)](1,2,collection.BitSet(1),s=3)

      (bfz1 === bfz1) must beTrue
      (bfz1 === bf1) must beTrue

      (bfz1 === bfz2) must beFalse
      (bfz1 === bfz3) must beFalse
      (bfz1 === bfz4) must beFalse
      (bfz1 === bf2) must beFalse
    }
    */

  }

  "a nonempty bloom filter" should {
    /*
    "is only equal to another nonempty bloom filter with same parameters and same bits set" in {
      val bf1: BloomFilter[String,(Long,Long)] =
        BFInstance[String,(Long,Long)](1,2,collection.BitSet(1),3)

      val bf2: BloomFilter[String,(Long,Long)] =
          BFInstance[String,(Long,Long)](0,2,collection.BitSet(1),3)

      val bf3: BloomFilter[String,(Long,Long)] =
          BFInstance[String,(Long,Long)](1,0,collection.BitSet(1),3)

      val bf4: BloomFilter[String,(Long,Long)] =
          BFInstance[String,(Long,Long)](1,2,collection.BitSet(1),0)

      val bf5: BloomFilter[String,(Long,Long)] =
          BFInstance[String,(Long,Long)](1,2,collection.BitSet(1,2),3)

      (bf1 === bf1) must beTrue

      (bf1 === bf2) must beFalse
      (bf1 === bf3) must beFalse
      (bf1 === bf4) must beFalse
      (bf1 === bf5) must beFalse

    }
    */
    "should contain all true positives" in {
      util.Random.setSeed(0)

      val BF = BloomFilter[String, (Long,Long)](100, 0.05) _
      0 to 10 foreach { i =>
        val items = 0 to 10 map { _ => util.Random nextDouble() toString }
        val bf = BF(items)

        items foreach { i => bf contains i must beTrue }
      }
    }

    "should be below false-positive rate with high confidence" in {
      util.Random.setSeed(0)

      Seq(0.1, 0.05, 0.01) foreach{ fpProb =>
        val fps = 0 until 10000 map { _ =>
          val numItems = 20
          val items = 0 until numItems map { _ => util.Random nextDouble() toString }
          val test = util.Random nextDouble() toString
          val bf = BloomFilter[String, (Long,Long)](numItems, fpProb)(items : _*)

          if(bf contains test) 1.0 else 0.0
        }
        val observed = fps.sum / fps.size

        observed must beLessThan(1.5 * fpProb)
      }
    }
  }
}
