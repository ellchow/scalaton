package scalaton.doo

import scala.util.{Random => SRandom}

import org.specs2.mutable._

import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.core.{Reduction, ScoobiConfiguration}
import com.nicta.scoobi.testing.mutable._

import scalaton.util.hashing32._

import scalaz.{DList => _, _}
import Scalaz._

import implicits._

class JoinsSpec extends HadoopSpecification {

  "bloom join should return the same DList as inner join" >>  { implicit conf: ScoobiConfiguration =>
    val leftSize = 100
    val rightSize = 100
    val leftMax = 20
    val rightMax = 20

    val left = DList((0 until leftSize).map(_ => SRandom.nextInt(leftMax)).zipWithIndex : _*)
    val right = DList((0 until rightSize).map(_ => SRandom.nextInt(rightMax)).zipWithIndex : _*)

     left.bloomJoin(right, 200).run.sorted must_== left.join(right).run.sorted
  }

  "skewed join should return the same DList as inner join" >>  { implicit conf: ScoobiConfiguration =>
    val leftSize = 100
    val rightSize = 100
    val leftMax = 20
    val rightMax = 20

    val left = DList((0 until leftSize).map(_ => SRandom.nextInt(leftMax)).zipWithIndex : _*)
    val right = DList((0 until rightSize).map(_ => SRandom.nextInt(rightMax)).zipWithIndex : _*)

    left.skewedJoin(right, 1.0, 3).run.sorted must_== left.join(right).run.sorted
  }


}
