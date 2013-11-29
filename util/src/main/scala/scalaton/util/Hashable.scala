/*
 Copyright 2013 Elliot Chow

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

package scalaton.util

import scalaz._
import Scalaz._

/**
 * Hashable type class
 */
trait Hashable[A,B]{
  def digest(a: A, seed: Long): B

  def multiDigest(a: A, seed: Long): Stream[B] =
    Stream.cons(digest(a, seed),
                multiDigest(a, seed + 1))
}

trait Hashable32Instances
extends HashFuncs{

  implicit def intHashable32 = new Hashable[Int, Int]{
    def digest(a: Int, seed: Long): Int =
      MurmurHash32(seed toInt)(a)
  }

  implicit def charHashable32 = new Hashable[Char, Int]{
    def digest(a: Char, seed: Long): Int =
      MurmurHash32(seed toInt)(a)
  }

  implicit def shortHashable32 = new Hashable[Short, Int]{
    def digest(a: Short, seed: Long): Int =
      MurmurHash32(seed toInt)(a)
  }

  implicit def floatHashable32 = new Hashable[Float, Int]{
    def digest(a: Float, seed: Long): Int =
      MurmurHash32(seed toInt)(a)
  }

  implicit def longHashable32 = new Hashable[Long, Int]{
    def digest(a: Long, seed: Long): Int =
      MurmurHash32(seed toInt)(a)
  }

  implicit def doubleHashable32 = new Hashable[Double, Int]{
    def digest(a: Double, seed: Long): Int =
      MurmurHash32(seed toInt)(a)
  }

  implicit def stringHashable32 = new Hashable[String, Int]{
    def digest(a: String, seed: Long): Int =
      MurmurHash32(seed toInt)(a)
  }

  implicit def seqHashable32[A,F[A] <: Seq[A]](implicit h: Hashable[A, Int]) = new Hashable[F[A], Int]{
    def digest(a: F[A], seed: Long): Int =
      a.foldLeft(1)((soFar, next) => combine32Hashes(soFar, h.digest(next, seed)))

  }

}

trait Hashable64Instances
extends HashFuncs{

  implicit def intHashable64 = new Hashable[Int, Long]{
    def digest(a: Int, seed: Long): Long =
      MurmurHash64(seed toInt)(a)
  }

  implicit def charHashable64 = new Hashable[Char, Long]{
    def digest(a: Char, seed: Long): Long =
      MurmurHash64(seed toInt)(a)
  }

  implicit def shortHashable64 = new Hashable[Short, Long]{
    def digest(a: Short, seed: Long): Long =
      MurmurHash64(seed toInt)(a)
  }

  implicit def floatHashable64 = new Hashable[Float, Long]{
    def digest(a: Float, seed: Long): Long =
      MurmurHash64(seed toInt)(a)
  }

  implicit def longHashable64 = new Hashable[Long, Long]{
    def digest(a: Long, seed: Long): Long =
      MurmurHash64(seed toInt)(a)
  }

  implicit def doubleHashable64 = new Hashable[Double, Long]{
    def digest(a: Double, seed: Long): Long =
      MurmurHash64(seed toInt)(a)
  }

  implicit def stringHashable64 = new Hashable[String, Long]{
    def digest(a: String, seed: Long): Long =
      MurmurHash64(seed toInt)(a)
  }

  implicit def seqHashable64[A,F[A] <: Seq[A]](implicit h: Hashable[A, Long]) = new Hashable[F[A], Long]{
    def digest(a: F[A], seed: Long): Long =
      a.foldLeft(1L)((soFar, next) => combine64Hashes(soFar, h.digest(next, seed)))

  }

}

trait Hashable128Instances
extends HashFuncs
{

  implicit def intHashable128 = new Hashable[Int, (Long, Long)]{
    def digest(a: Int, seed: Long): (Long, Long) =
      MurmurHash128(seed)(a)
  }

  implicit def charHashable128 = new Hashable[Char, (Long, Long)]{
    def digest(a: Char, seed: Long): (Long, Long) =
      MurmurHash128(seed)(a)
  }

  implicit def shortHashable128 = new Hashable[Short, (Long, Long)]{
    def digest(a: Short, seed: Long): (Long, Long) =
      MurmurHash128(seed)(a)
  }

  implicit def floatHashable128 = new Hashable[Float, (Long, Long)]{
    def digest(a: Float, seed: Long): (Long, Long) =
      MurmurHash128(seed)(a)
  }

  implicit def longHashable128 = new Hashable[Long, (Long, Long)]{
    def digest(a: Long, seed: Long): (Long, Long) =
      MurmurHash128(seed)(a)
  }

  implicit def doubleHashable128 = new Hashable[Double, (Long, Long)]{
    def digest(a: Double, seed: Long): (Long, Long) =
      MurmurHash128(seed)(a)
  }

  implicit def stringHashable128 = new Hashable[String, (Long, Long)]{
    def digest(a: String, seed: Long): (Long, Long) =
      MurmurHash128(seed)(a)
  }

  implicit def seqHashable128[A,F[A] <: Seq[A]](implicit h: Hashable[A, (Long,Long)]) = new Hashable[F[A], (Long,Long)]{
    def digest(a: F[A], seed: Long): (Long,Long) =
      a.foldLeft((1L,1L))((soFar, next) => combine128Hashes(soFar, h.digest(next, seed)))
  }

}


/**
 * HashCode Converter type class for converting between types (e.g. Long to Int)
 */
trait HashCodeConverter[A, B]{
  def convert(hc: A): Seq[B]

  def convertSeq(hcs: Stream[A]): Iterable[B] =
    hcs flatMap{ hc => convert(hc) }

}


/** Typeclasses for converting hash codes **/
trait HashCodeConverterInstances{
  implicit def hashCodeIdentity[A] = new HashCodeConverter[A, A]{
    def convert(hc: A): Seq[A] = Seq(hc)
  }

  implicit val hashCodeLongToInt = new HashCodeConverter[Long, Int]{
    def convert(hc: Long): Seq[Int] =
      Tag subst Seq(math.abs(hc >> 32).toInt, math.abs((hc << 32) >> 32).toInt)

  }

  implicit val hashCodeLongLongToLong = new HashCodeConverter[(Long,Long), Long]{
    def convert(hc: (Long, Long)): Seq[Long] =
      Tag subst Seq(hc._1, hc._2)
  }


  implicit val hashCodeLongLongToInt = new HashCodeConverter[(Long,Long), Int]{
    def convert(hc: (Long, Long)): Seq[Int] =
      Tag subst Seq(math.abs(hc._1 >> 32).toInt, math.abs((hc._1 << 32) >> 32).toInt,
                    math.abs(hc._2 >> 32).toInt, math.abs((hc._2 << 32) >> 32).toInt)

  }

}

trait HashableFunctions{

  def hash[A,B](a: A, seed: Long = 0L)
               (implicit h: Hashable[A,B]): B =
    h.digest(a, seed)

  def multiHash[A,B](a: A, seed: Long)
                    (implicit h: Hashable[A,B]): Stream[B] =
    h.multiDigest(a, seed)
}


object hashing
extends HashableFunctions
with HashCodeConverterInstances

object hashing32
extends HashableFunctions
with HashCodeConverterInstances
with Hashable32Instances
with HashableTuple32Instances{
  type Bits32 = Int
}

object hashing64
extends HashableFunctions
with HashCodeConverterInstances
with Hashable64Instances
with HashableTuple64Instances{
  type Bits64 = Long
}

object hashing128
extends HashableFunctions
with HashCodeConverterInstances
with Hashable128Instances
with HashableTuple128Instances{
  type Bits128 = (Long, Long)
}
