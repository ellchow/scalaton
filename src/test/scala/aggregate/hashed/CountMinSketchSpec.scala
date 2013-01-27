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


  }

  "a nonempty count min sketch" should {
  }

}
