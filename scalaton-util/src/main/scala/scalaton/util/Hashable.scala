package scalaton.util

import scalaz._
import Scalaz._


/**
 * Hashable type class, implicits,  and related things
 */

sealed trait HashCode
trait HashingTags{
  def HashCode [A](a: A) = Tag[A,HashCode](a)
}

/**
 * Hashable type class
 */
trait Hashable[A,B] extends HashingTags{
  def digest(a: A, seed: Long): B @@ HashCode

  def multiDigest(a: A, seed: Long): Stream[B @@ HashCode] =
    Stream.cons(digest(a, seed),
                multiDigest(a, seed + 1))
}

trait Hashable32Instances
extends HashFuncs
with HashingTags{

  implicit def intHashable32 = new Hashable[Int, Int]{
    def digest(a: Int, seed: Long): Int @@ HashCode =
      HashCode(MurmurHash32(seed toInt)(a))
  }

  implicit def charHashable32 = new Hashable[Char, Int]{
    def digest(a: Char, seed: Long): Int @@ HashCode =
      HashCode(MurmurHash32(seed toInt)(a))
  }

  implicit def shortHashable32 = new Hashable[Short, Int]{
    def digest(a: Short, seed: Long): Int @@ HashCode =
      HashCode(MurmurHash32(seed toInt)(a))
  }

  implicit def floatHashable32 = new Hashable[Float, Int]{
    def digest(a: Float, seed: Long): Int @@ HashCode =
      HashCode(MurmurHash32(seed toInt)(a))
  }

  implicit def longHashable32 = new Hashable[Long, Int]{
    def digest(a: Long, seed: Long): Int @@ HashCode =
      HashCode(MurmurHash32(seed toInt)(a))
  }

  implicit def doubleHashable32 = new Hashable[Double, Int]{
    def digest(a: Double, seed: Long): Int @@ HashCode =
      HashCode(MurmurHash32(seed toInt)(a))
  }

  implicit def stringHashable32 = new Hashable[String, Int]{
    def digest(a: String, seed: Long): Int @@ HashCode =
      HashCode(MurmurHash32(seed toInt)(a))
  }
}

trait Hashable64Instances
extends HashFuncs
with HashingTags{

  implicit def intHashable64 = new Hashable[Int, Long]{
    def digest(a: Int, seed: Long): Long @@ HashCode =
      HashCode(MurmurHash64(seed toInt)(a))
  }

  implicit def charHashable64 = new Hashable[Char, Long]{
    def digest(a: Char, seed: Long): Long @@ HashCode =
      HashCode(MurmurHash64(seed toInt)(a))
  }

  implicit def shortHashable64 = new Hashable[Short, Long]{
    def digest(a: Short, seed: Long): Long @@ HashCode =
      HashCode(MurmurHash64(seed toInt)(a))
  }

  implicit def floatHashable64 = new Hashable[Float, Long]{
    def digest(a: Float, seed: Long): Long @@ HashCode =
      HashCode(MurmurHash64(seed toInt)(a))
  }

  implicit def longHashable64 = new Hashable[Long, Long]{
    def digest(a: Long, seed: Long): Long @@ HashCode =
      HashCode(MurmurHash64(seed toInt)(a))
  }

  implicit def doubleHashable64 = new Hashable[Double, Long]{
    def digest(a: Double, seed: Long): Long @@ HashCode =
      HashCode(MurmurHash64(seed toInt)(a))
  }

  implicit def stringHashable64 = new Hashable[String, Long]{
    def digest(a: String, seed: Long): Long @@ HashCode =
      HashCode(MurmurHash64(seed toInt)(a))
  }
}

trait Hashable128Instances
extends HashFuncs
with HashingTags{

  implicit def intHashable128 = new Hashable[Int, (Long, Long)]{
    def digest(a: Int, seed: Long): (Long, Long) @@ HashCode =
      HashCode(MurmurHash128(seed)(a))
  }

  implicit def charHashable128 = new Hashable[Char, (Long, Long)]{
    def digest(a: Char, seed: Long): (Long, Long) @@ HashCode =
      HashCode(MurmurHash128(seed)(a))
  }

  implicit def shortHashable128 = new Hashable[Short, (Long, Long)]{
    def digest(a: Short, seed: Long): (Long, Long) @@ HashCode =
      HashCode(MurmurHash128(seed)(a))
  }

  implicit def floatHashable128 = new Hashable[Float, (Long, Long)]{
    def digest(a: Float, seed: Long): (Long, Long) @@ HashCode =
      HashCode(MurmurHash128(seed)(a))
  }

  implicit def longHashable128 = new Hashable[Long, (Long, Long)]{
    def digest(a: Long, seed: Long): (Long, Long) @@ HashCode =
      HashCode(MurmurHash128(seed)(a))
  }

  implicit def doubleHashable128 = new Hashable[Double, (Long, Long)]{
    def digest(a: Double, seed: Long): (Long, Long) @@ HashCode =
      HashCode(MurmurHash128(seed)(a))
  }

  implicit def stringHashable128 = new Hashable[String, (Long, Long)]{
    def digest(a: String, seed: Long): (Long, Long) @@ HashCode =
      HashCode(MurmurHash128(seed)(a))
  }
}


/**
 * HashCode Converter type class for converting between types (e.g. Long to Int)
 */
trait HashCodeConverter[A, B] extends HashingTags{
  def convert(hc: A @@ HashCode): Seq[B @@ HashCode]

  def convertSeq(hcs: Stream[A @@ HashCode]): Iterable[B @@ HashCode] =
    hcs flatMap{ hc => convert(hc) }

}


/** Typeclasses for converting hash codes **/
trait HashCodeConverterInstances extends HashingTags{
  implicit def hashCodeIdentity[A] = new HashCodeConverter[A, A]{
    def convert(hc: A @@ HashCode): Seq[A @@ HashCode] = Seq(hc)
  }

  implicit val hashCodeLongToInt = new HashCodeConverter[Long, Int]{
    def convert(hc: Long @@ HashCode): Seq[Int @@ HashCode] =
      Tag subst Seq(math.abs(hc >> 32).toInt, math.abs((hc << 32) >> 32).toInt)

  }

  implicit val hashCodeLongLongToLong = new HashCodeConverter[(Long,Long), Long]{
    def convert(hc: (Long, Long) @@ HashCode): Seq[Long @@ HashCode] =
      Tag subst Seq(hc._1, hc._2)
  }


  implicit val hashCodeLongLongToInt = new HashCodeConverter[(Long,Long), Int]{
    def convert(hc: (Long, Long) @@ HashCode): Seq[Int @@ HashCode] =
      Tag subst Seq(math.abs(hc._1 >> 32).toInt, math.abs((hc._1 << 32) >> 32).toInt,
                    math.abs(hc._2 >> 32).toInt, math.abs((hc._2 << 32) >> 32).toInt)

  }

}

trait HashableFunctions extends HashingTags{

  def hash[A,B](a: A, seed: Long = 0L)
               (implicit h: Hashable[A,B]): B @@ HashCode =
    h.digest(a, seed)

  def multiHash[A,B](a: A, seed: Long)
                    (implicit h: Hashable[A,B]): Stream[B @@ HashCode] =
    h.multiDigest(a, seed)

}

trait HashingSizes{
  type Bits128 = (Long, Long)
  type Bits64 = Long
  type Bits32 = Int
}

object hashing
extends HashableFunctions
with HashCodeConverterInstances
with HashingTags
with HashingSizes

object hashing32
extends Hashable32Instances
with HashableTuple32Instances

object hashing64
extends Hashable64Instances
with HashableTuple64Instances

object hashing128
extends Hashable128Instances
with HashableTuple128Instances
