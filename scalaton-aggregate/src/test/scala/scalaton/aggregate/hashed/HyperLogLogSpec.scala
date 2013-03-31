package scalaton.aggregate.hashed

import scala.collection.BitSet
import scala.collection.mutable.{BitSet => MBitSet}
import com.googlecode.javaewah.{EWAHCompressedBitmap => CompressedBitSet}

import scala.util.{Random => SRandom}

import org.specs2.mutable._

import scalaz._
import Scalaz._

import scalaton.util.hashing._
import scalaton.util.hashing128._
import scalaton.aggregate.hashed.hcollection._
import scalaton.aggregate.hashed.hyperloglog._

class HyperLogLogSpec extends Specification{
  trait DHYLL
  trait SHYLL

  def tagDense[A](a: A) = Tag[A, DHYLL](a)
  def tagSparse[A](a: A) = Tag[A, SHYLL](a)

  "a hyper log log estimator" should {

    def testErrorRate[D](hllinst: HyperLogLogT[String,Bits128,D]){
      implicit val hllinstance = hllinst


      val (err, h) = (1 to 10000).foldLeft((List[Double](), hllinstance.zero))((acc, x) => {
        val u = insert(acc._2, x toString)
        val e = cardinality(u)

        ((math.abs(x - e).toDouble / x) :: acc._1,u)
      })

      (err.sum / err.size) must beLessThan(hll.error(hllinstance.m))
    }

    "track cardinality with reasonable error rate" in {
      Seq(7,8,9) foreach( m => testErrorRate(hll.dense[String,Bits128,DHYLL](m)) )

      Seq(7,10,16) foreach( m => testErrorRate(hll.sparse[String,Bits128,SHYLL](m)) )
    }
  }


}
