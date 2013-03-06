package scalaton.aggregate.hashed

import scala.collection.BitSet
import scala.collection.mutable.{BitSet => MBitSet}
import com.googlecode.javaewah.{EWAHCompressedBitmap => CompressedBitSet}

import scala.util.{Random => SRandom}

import org.specs2.mutable._

import scalaz._
import Scalaz._

import scalaton.util.hashing._
import scalaton.aggregate.hashed.hcollection._
import scalaton.aggregate.hashed.bloomfilter.sbf
import scalaton.aggregate.hashed.mutable.bloomfilter.{sbf => msbf}

class StandardBloomFilterSpec extends Specification{
  trait DSBF
  trait SSBF

  def tagDense[A](a: A) = Tag[A, DSBF](a)
  def tagSparse[A](a: A) = Tag[A, SSBF](a)

  "an empty bloom filter" should {

    implicit val sbfinstance = sbf[String,Bits128,DSBF]((5,625), 0L)

    "not contain anything" in {
      SRandom.setSeed(0)

      0 to 1000 foreach { i =>
         contains(sbfinstance.zero,
                  SRandom nextDouble() toString) must beFalse
      }
    }

    "is only equal to another empty bloom filter" in {
      val bfz = tagDense(BitSet.empty)
      (sbfinstance.zero === bfz) must beTrue

      0 to 1000 foreach { i =>
        (sbfinstance.zero === singleton(tagDense(SRandom nextDouble() toString))) must beFalse
      }
    }

    "be empty when added with another empty bloom filter" in {
      ((sbfinstance.zero |+| sbfinstance.zero) === sbfinstance.zero) must beTrue
    }

    "contain the item after adding it" in {
      contains(singleton(tagDense("a")), "a") must beTrue
    }

    "be equal to the other bloom filter after add another" in {
      ((sbfinstance.zero |+| singleton(tagDense("a"))) === singleton(tagDense("a"))) must beTrue
    }

  }

  "a nonempty bloom filter" should {

    def testTruePositives[D](sbfinst: StandardBloomFilterT[String,Bits128,D]) = {
      SRandom.setSeed(0)
      implicit val sbfinstance = sbfinst

      0 to 10 foreach { i =>
        val items = 0 to 10 map { _ => SRandom nextDouble() toString }
        val bf = items.foldLeft(sbfinstance.zero)((acc,x) => insert(acc,x))

        items foreach { i => contains(bf, i) must beTrue }
      }
    }

    def testFPProb[D](sbfinst: StandardBloomFilterT[(String,String),Bits128,D],
                      numItems: Int, fpProb: Double) = {
      SRandom.setSeed(0)
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
