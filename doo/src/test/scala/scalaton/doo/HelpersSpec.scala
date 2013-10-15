package scalaton.doo

import scala.util.{Random => SRandom}

import org.specs2.mutable._

import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.core.{Reduction, ScoobiConfiguration}
import com.nicta.scoobi.testing.mutable._

import scalaton.util.hashing._
import scalaton.util.hashing32._

import scalaz.{DList => _, _}
import Scalaz._

import implicits._

class HelpersSpec extends HadoopSpecification {

  "groupByKeyThenCombine should yield same results as groupByKey.combine" >>  { implicit conf: ScoobiConfiguration =>
    for(_ <- 1 to 10)
    yield{
      val n = SRandom.nextInt(10000) + 1
      val m = SRandom.nextInt(1000) + 1
      val flushThreshold = SRandom.nextInt(n) + 1

      val dl = DList((0 until n).map(_ => SRandom.nextInt()).zipWithIndex : _*)

      dl.groupByKeyThenCombine(_.size gte flushThreshold).run.sorted must_== dl.groupByKey.combine((_: Int) |+| (_: Int)).run.sorted
    }
  }
}
