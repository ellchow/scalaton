package scalaton.aggregate.hashed

import scala.collection.BitSet

import scala.util.{Random => SRandom}

import org.specs2.mutable._

import scalaz.{BloomFilter => _, _}
import Scalaz._

import scalaton.util.hashable._
import scalaton.aggregate.hashed.bloomfilter._

class StandardBloomFilterSpec extends Specification{

  "an empty bloom filter" should {

    implicit val bfinstance = StandardBloomFilter[String,(Long,Long)]((5,625), 0L)

    "not contain anything" in {
      SRandom.setSeed(0)

      0 to 1000 foreach { i =>
         contains(StandardBloomFilter.empty,
                  SRandom nextDouble() toString) must beFalse
      }
    }

    "is only equal to another empty bloom filter" in {
      val bfz = Tag[BitSet,BF](BitSet.empty)
      (StandardBloomFilter.empty === bfz) must beTrue

      0 to 1000 foreach { i =>
        (StandardBloomFilter.empty === singleton(SRandom nextDouble() toString)) must beFalse
      }
    }

    "be empty when added with another empty bloom filter" in {
      ((StandardBloomFilter.empty |+| StandardBloomFilter.empty) === StandardBloomFilter.empty) must beTrue
    }

    "contain the item after adding it" in {
      contains(singleton("a"), "a") must beTrue
    }

    "be equal to the other bloom filter after add another" in {
      ((StandardBloomFilter.empty |+| singleton("a")) === singleton("a")) must beTrue
    }

  }

  "a nonempty bloom filter" should {

    "should contain all true positives" in {
      SRandom.setSeed(0)

      val params = StandardBloomFilter.optimalParameters(100, 0.05)
      implicit val BF = StandardBloomFilter[String, (Long,Long)](params, 0L)

      0 to 10 foreach { i =>
        val items = 0 to 10 map { _ => SRandom nextDouble() toString }
        val bf = items.foldLeft(StandardBloomFilter.empty)((acc,x) => insert(acc,x))

        items foreach { i => contains(bf, i) must beTrue }
      }
    }

    "should be below false-positive rate with high confidence" in {
      SRandom.setSeed(0)

      Seq(0.1, 0.05, 0.01, 0.005) foreach{ fpProb =>
        val numItems = 20
        val params = StandardBloomFilter.optimalParameters(numItems, fpProb)

        implicit val BF = StandardBloomFilter[(String, String), (Long,Long)](params, 0L)

        val fps = 0 until 10000 map { _ =>
          val items = 0 until numItems map { _ => (SRandom nextDouble() toString,
                                                   SRandom nextDouble() toString) }
          val test = (SRandom nextDouble() toString, SRandom nextDouble() toString)

          val bf = items.foldLeft(StandardBloomFilter.empty)((acc,x) => insert(acc,x))
          if(contains(bf, test)) 1.0 else 0.0
        }
        val observed = fps.sum / fps.size

        observed must beLessThan(1.5 * fpProb)
      }
    }

    "should estimate size well for elements less than the intended number of elements" in {
      implicit val sbfinstance = StandardBloomFilter[String,(Long,Long)](StandardBloomFilter.optimalParameters(100,0.05),0)

      var bf = StandardBloomFilter.empty
      for(i <- 1 to 100){
        bf = insert(bf, scala.util.Random.nextDouble.toString)

        math.abs(cardinality(bf) - i) must beLessThan(math.max(math.round(1.05 * i), 1).toLong)
      }
    }

    "should should return cardinality of -1 if all bloom filter is full" in {
      implicit val sbfinstance = StandardBloomFilter[String,(Long,Long)](StandardBloomFilter.optimalParameters(10,0.05),0)

      val bf: SBF = Tag[BitSet,BF](BitSet((0 until sbfinstance.width) : _*))

      (cardinality(bf) === -1L) must beTrue
    }
  }
}
