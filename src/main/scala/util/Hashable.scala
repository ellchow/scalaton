package scalaton.util

import collection.mutable

import scalaz._
import Scalaz._

sealed trait HashCode

sealed trait HashSeed

trait HashingTags{
  def HashCode [A](a: A) = Tag[A,HashCode](a)

  def HashSeed[A](a: A) = Tag[A,HashSeed](a)
}


trait Hashable[A,B] extends HashingTags{
  def digest(a: A, seed: Long @@ HashSeed = HashSeed(0L)): B @@ HashCode

  def multiDigest(a: A, seed: Long @@ HashSeed): Stream[B @@ HashCode] =
    Stream.cons(digest(a, HashSeed(seed)),
                multiDigest(a, HashSeed(seed + 1)))
}

trait LowPriorityHashableInstances extends HashingTags{
  implicit def anyHashable[A] = new Hashable[A, Long]{
    def digest(a: A, seed: Long @@ HashSeed = HashSeed(0L)): Long @@ HashCode = HashCode(a.hashCode.toLong)
  }
}

trait HashableInstances extends HashFuncs with LowPriorityHashableInstances with HashingTags{
  implicit def intHashable128 = new Hashable[Int, (Long, Long)]{
    def digest(a: Int, seed: Long @@ HashSeed): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def charHashable128 = new Hashable[Char, (Long, Long)]{
    def digest(a: Char, seed: Long @@ HashSeed): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def shortHashable128 = new Hashable[Short, (Long, Long)]{
    def digest(a: Short, seed: Long @@ HashSeed): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def floatHashable128 = new Hashable[Float, (Long, Long)]{
    def digest(a: Float, seed: Long @@ HashSeed): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def longHashable128 = new Hashable[Long, (Long, Long)]{
    def digest(a: Long, seed: Long @@ HashSeed): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def doubleHashable128 = new Hashable[Double, (Long, Long)]{
    def digest(a: Double, seed: Long @@ HashSeed): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def stringHashable128 = new Hashable[String, (Long, Long)]{
    def digest(a: String, seed: Long @@ HashSeed): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }
}

trait HashCodeConverter[A, B] extends HashingTags{
  def convert(hc: A @@ HashCode): Seq[B @@ HashCode]
  def convertSeq(hcs: Seq[A @@ HashCode]): Iterable[B @@ HashCode]=
    new Iterable[B @@ HashCode]{
      def iterator = new Iterator[B @@ HashCode]{
        private val buf: mutable.ArrayBuffer[B @@ HashCode] = mutable.ArrayBuffer.empty

        def hasNext = buf.nonEmpty || hcs.nonEmpty

        def next = {
          if(buf.isEmpty)
            buf ++= convert(hcs.head)
          val x = buf.head
          buf.drop(1)
          x
        }
      }
    }

}

trait HashCodeConverterInstances extends HashingTags{
  implicit val hashCodeLongToInt = new HashCodeConverter[Long, Int]{
    def convert(hc: Long @@ HashCode): Seq[Int @@ HashCode] =
      Tag subst Seq(math.abs(hc >> 32).toInt, math.abs((hc << 32) >> 32).toInt)

  }

  implicit val hashCodeLongLongToLong = new HashCodeConverter[(Long,Long), Long]{
    def convert(hc: (Long, Long) @@ HashCode): Seq[Long @@ HashCode] =
      Tag subst Seq(hc._1, hc._2)
  }

  implicit val hashCodeLongLongToInt = new HashCodeConverter[(Long,Long), Int]{
    def convert(hc: (Long, Long) @@ HashCode): Seq[Int @@ HashCode] = {
      Tag subst Seq(math.abs(hc._1 >> 32).toInt, math.abs((hc._1 << 32) >> 32).toInt,
                    math.abs(hc._2 >> 32).toInt, math.abs((hc._2 << 32) >> 32).toInt)
    }
  }
}



trait HashableFunctions extends HashingTags{

  def hash[A,B](a: A, seed: Long @@ HashSeed = HashSeed(0L))(implicit h: Hashable[A,B]): B @@ HashCode =
    h.digest(a, seed)

  def multiHash[A,B](a: A, seed: Long @@ HashSeed)(implicit h: Hashable[A,B]): Stream[B @@ HashCode] =
    h.multiDigest(a, seed)

}

object hashable
extends HashableInstances
with HashableFunctions
with HashCodeConverterInstances
with HashingTags




