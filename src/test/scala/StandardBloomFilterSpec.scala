package scalaton

import scala.util.{Random => SRandom}

import org.specs2.mutable._

import scalaz.{BloomFilter => _, _}
import Scalaz._

import scalaton.util.hashable._
import scalaton.StandardBloomFilter

class StandardBloomFilterSpec extends Specification{

  "an empty bloom filter" should {

    implicit val bfmon = StandardBloomFilter.monoid[String,(Long,Long)]((5,625))
    val bfz: StandardBloomFilter[String,(Long,Long)] = BFZero[String,(Long,Long)](5, 625)

    "not contain anything" in {
      SRandom.setSeed(0)

      0 to 1000 foreach { i =>
        bfz contains (SRandom nextDouble() toString) must beFalse
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

    "is only equal to empty bloom filters with same parameters and without any bits set" in {
      implicit val bfmon = StandardBloomFilter.monoid[String,(Long,Long)]((1,2),3)

      val bfz1: StandardBloomFilter[String,(Long,Long)] =
        BFZero(1, 2, 3)

      val bfz2: StandardBloomFilter[String,(Long,Long)] =
        BFZero(100, 2, 3)

      val bfz3: StandardBloomFilter[String,(Long,Long)] =
        BFZero(1, 100, 3)

      val bfz4: StandardBloomFilter[String,(Long,Long)] =
        BFZero(1, 2, 100)

      val bf1: StandardBloomFilter[String,(Long,Long)] =
        BFInstance[String,(Long,Long)](1,2,collection.BitSet.empty,3)

      val bf2: StandardBloomFilter[String,(Long,Long)] =
        BFInstance[String,(Long,Long)](1,2,collection.BitSet(1),3)

      (bfz1 === bfz1) must beTrue
      (bfz1 === bf1) must beTrue

      (bfz1 === bf2) must beFalse

      (bfz1 === bfz2) must throwA[IllegalArgumentException]
      (bfz1 === bfz3) must throwA[IllegalArgumentException]
      (bfz1 === bfz4) must throwA[IllegalArgumentException]
    }

  }

  "a nonempty bloom filter" should {

    "is only equal to another nonempty bloom filter with same parameters and same bits set" in {
      implicit val bfmon = StandardBloomFilter.monoid[String,(Long,Long)]((1,2),3)

      val bf1: StandardBloomFilter[String,(Long,Long)] =
        BFInstance[String,(Long,Long)](1,2,collection.BitSet(1),3)

      val bf2: StandardBloomFilter[String,(Long,Long)] =
          BFInstance[String,(Long,Long)](100,2,collection.BitSet(1),3)

      val bf3: StandardBloomFilter[String,(Long,Long)] =
          BFInstance[String,(Long,Long)](1,100,collection.BitSet(1),3)

      val bf4: StandardBloomFilter[String,(Long,Long)] =
          BFInstance[String,(Long,Long)](1,2,collection.BitSet(1),100)

      val bf5: StandardBloomFilter[String,(Long,Long)] =
          BFInstance[String,(Long,Long)](1,2,collection.BitSet(1,2),3)

      (bf1 === bf1) must beTrue
      (bf1 === bf5) must beFalse

      (bf1 === bf2) must throwA[IllegalArgumentException]
      (bf1 === bf3) must throwA[IllegalArgumentException]
      (bf1 === bf4) must throwA[IllegalArgumentException]
    }

    "should contain all true positives" in {
      SRandom.setSeed(0)

      val BF = StandardBloomFilter[String, (Long,Long)](100, 0.05) _
      0 to 10 foreach { i =>
        val items = 0 to 10 map { _ => SRandom nextDouble() toString }
        val bf = BF(items)

        items foreach { i => bf contains i must beTrue }
      }
    }

    "should be below false-positive rate with high confidence" in {
      SRandom.setSeed(0)

      Seq((0.1, 0.15), (0.05, 0.075),
          (0.01,0.015), (0.005, 0.01)
        ) foreach{ case (fpProb, maxFp) =>
        val fps = 0 until 15000 map { _ =>
          val numItems = 20
          // val items = 0 until numItems map { _ => SRandom nextDouble() toString }
          // val test = SRandom nextDouble() toString
          // val bf = StandardBloomFilter[String, (Long,Long)](numItems, fpProb)(items : _*)

          val items = 0 until numItems map { _ => (SRandom nextDouble() toString,
                                                   SRandom nextDouble() toString) }
          val test = (SRandom nextDouble() toString, SRandom nextDouble() toString)
          val bf = StandardBloomFilter[(String, String), (Long,Long)](numItems, fpProb)(items : _*)

          if(bf contains test) 1.0 else 0.0
        }
        val observed = fps.sum / fps.size

        observed must beLessThan(maxFp)
      }
    }
  }
}
