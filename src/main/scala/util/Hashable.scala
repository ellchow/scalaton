package scalaton.util

import scalaz._
import Scalaz._

sealed trait HashCode
sealed trait HashSeed

object HashCode{
  def apply[A](a: A) = Tag[A,HashCode](a)
}
object HashSeed{
  def apply[A](a: A) = Tag[A,HashSeed](a)
}


trait Hashable[A,B]{
  def digest(a: A, seed: Long @@ HashSeed = HashSeed(0L)): B @@ HashCode

  def multiDigest(a: A, seed: Long @@ HashSeed): Stream[B @@ HashCode] =
    Stream.cons(digest(a, HashSeed(seed)),
                multiDigest(a, HashSeed(seed + 1)))
}

trait LowPriorityHashableInstances{
  implicit def anyHashable[A] = new Hashable[A, Long]{
    def digest(a: A, seed: Long @@ HashSeed = HashSeed(0L)): Long @@ HashCode = HashCode(a.hashCode.toLong)
  }
}

trait HashableInstances extends HashFuncs with LowPriorityHashableInstances{

  implicit def intHashable = new Hashable[Int, (Long, Long)]{
    def digest(a: Int, seed: Long @@ HashSeed): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def charHashable = new Hashable[Char, (Long, Long)]{
    def digest(a: Char, seed: Long @@ HashSeed): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def shortHashable = new Hashable[Short, (Long, Long)]{
    def digest(a: Short, seed: Long @@ HashSeed): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def floatHashable = new Hashable[Float, (Long, Long)]{
    def digest(a: Float, seed: Long @@ HashSeed): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def longHashable = new Hashable[Long, (Long, Long)]{
    def digest(a: Long, seed: Long @@ HashSeed): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def doubleHashable = new Hashable[Double, (Long, Long)]{
    def digest(a: Double, seed: Long @@ HashSeed): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def stringHashable = new Hashable[String, (Long, Long)]{
    def digest(a: String, seed: Long @@ HashSeed): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }
}

trait HashableFunctions{

  def hash[A,B](a: A, seed: Long @@ HashSeed = HashSeed(0L))(implicit h: Hashable[A,B]): B @@ HashCode =
    h.digest(a, seed)

  def multiHash[A,B](a: A, seed: Long @@ HashSeed)(implicit h: Hashable[A,B]): Stream[B @@ HashCode] =
    h.multiDigest(a, seed)
}

object hashable extends HashableInstances with HashableFunctions




