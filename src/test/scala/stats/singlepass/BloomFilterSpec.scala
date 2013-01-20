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

  }

  "a nonempty bloom filter" should {


    "should contain all true positives" in {
      util.Random.setSeed(0)

      val BF = BloomFilter[String, (Long,Long)](100, 0.05) _
      0 to 10 foreach { i =>
        val items = 0 to 10 map { _ => util.Random nextDouble() toString }
        val bf = BF(items)

        items foreach { i =>
          bf contains i must beTrue
        }
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
