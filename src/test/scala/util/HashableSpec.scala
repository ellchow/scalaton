package scalaton.util

import org.specs2.mutable._
import scalaz._
import Scalaz._

import scalaton.util.hashable._

class HashableSpec extends Specification {

  "The string 'hello'" should {
    "can use default hashable instance" in {
      implicit val hashAnything = anyHashable[String]
      val hc: (Long,Long) @@ HashCode = hash("hello")

      hc mustEqual (5620873505596599413L, 2937734076964678877L)
    }

    "is hashed by MurmurHash3" in {
      val hc = hash("hello")
      (hc: (Long, Long)).productArity mustEqual 2

      hc mustEqual (-4758432102323878981L,1262627326183304356L)
    }

    "have different hash codes given different seeds" in {
      val n = 100L
      val seeds: Seq[Long] = 0L until n
      val hcs = seeds map {i => hash("hello", i)} toSet

      hcs.size mustEqual n
    }

    "can be hashed multiple times" in {
      val n = 100
      val hcs = multiHash("hello", 0L) take n

      hcs.size mustEqual n
    }

    "yields mostly distinct values when hashing many times" in {
      val n = 1000
      val hcs = multiHash("hello", 0L) take n distinct

      hcs.size must beGreaterThan((0.99 * n) toInt)
    }

  }



}


    // multiHash("hello").take(3).foreach( _ |> println )

    // multiHash(Map(1->2))(serHashable).take(3).foreach( _ |> println )
