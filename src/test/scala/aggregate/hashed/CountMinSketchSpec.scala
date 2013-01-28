/*
package scalaton.aggregate.hashed

import scala.util.{Random => SRandom}

import org.specs2.mutable._

import scalaz._
import Scalaz._

import scalaton.util.hashable._
import scalaton.aggregate.hashed.sketch._

class CountMinSketchSpec extends Specification{
  "an empty count min sketch" should {
    implicit val cmsinstance = CountMinSketch[String,(Long,Long)]((5,60),0)

    "not have positive counts for anything" in {
      SRandom.setSeed(0)

      0 to 1000 foreach { i =>
        lookup(cmsinstance.zero,
               SRandom nextDouble() toString) mustEqual 0L
      }
    }

    "is only equal to another empty count min sketch" in {
      val cmsz = CountMinSketch.CMS((Vector.fill[Long](5, 60)(0L), 0L))
      (cmsinstance.zero === cmsz) must beTrue

      0 to 1000 foreach { i =>
        (cmsinstance.zero === singleton(SRandom nextDouble() toString, 1L)) must beFalse
      }
    }

    "be empty when added with another empty count min sketch" in {
      ((cmsinstance.zero |+| cmsinstance.zero) === cmsinstance.zero) must beTrue
    }

    "have an value for an item after updating it" in {
      lookup(singleton("a", 1L), "a") mustEqual 1L
    }

    "be equal to the other count min sketch after updating it" in {
      ((cmsinstance.zero |+| singleton("a",1L)) === singleton("a",1L)) must beTrue
    }

  }

  "a nonempty count min sketch" should {

    "should have nonzero estimate for any item that has been updated" in {
      SRandom.setSeed(0)

      implicit val cmsinstance = CountMinSketch[String,(Long,Long)]((5,60),0)

      0 to 10 foreach { i =>
        val items = 0 to 10 map { _ => SRandom nextDouble() toString }
        val cms = items.foldLeft(cmsinstance.zero)((acc,x) => update(acc, x, 1L))

        items foreach { i => (lookup(cms, i) > 0) must beTrue }
      }
    }

    "should be within error bounds" in {
      SRandom.setSeed(0)

      for{ eps <- Seq(0.05, 0.01, 0.001)
           delta <- Seq(0.95, 0.999)
         }{
        val params = CountMinSketch.optimalParameters(eps,delta)
        implicit val cmsinstance = CountMinSketch[String,(Long,Long)](params,0)

        val items = (0 until 5000).view.map( _ => (math.sqrt(SRandom.nextInt(10000)).toInt.toString,
                                                    SRandom.nextInt(20).toLong) )
        val trueCounts = items groupBy (_._1) map { case (k, vs) => (k, vs.size) } toMap

        val (i,c) = items.head

        val cms = items.foldLeft(cmsinstance.zero){ case (acc, (i,c)) => update(acc, i, c)}

        val exceedsMaxError = items map { case (i,c) => (lookup(cms, i) < (1 + eps) * trueCounts.getOrElse(i,0)) ? 1 | 0 } sum

        (exceedsMaxError.toDouble / items.size) must beLessThan(delta)
      }
    }

  }

}
*/
