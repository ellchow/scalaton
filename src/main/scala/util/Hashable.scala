package scalaton.util

import scala.language.postfixOps

import collection.mutable

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

trait LowPriorityHashableInstances extends HashingTags{
  implicit def anyHashable[A] = new Hashable[A, Long]{
    def digest(a: A, seed: Long = 0L): Long @@ HashCode =
      HashCode(a.hashCode.toLong)
  }
}

trait HashableInstances
extends HashFuncs
with HashableTupleInstances
with LowPriorityHashableInstances
with HashingTags{

  implicit def intHashable128 = new Hashable[Int, (Long, Long)]{
    def digest(a: Int, seed: Long): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def charHashable128 = new Hashable[Char, (Long, Long)]{
    def digest(a: Char, seed: Long): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def shortHashable128 = new Hashable[Short, (Long, Long)]{
    def digest(a: Short, seed: Long): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def floatHashable128 = new Hashable[Float, (Long, Long)]{
    def digest(a: Float, seed: Long): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def longHashable128 = new Hashable[Long, (Long, Long)]{
    def digest(a: Long, seed: Long): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def doubleHashable128 = new Hashable[Double, (Long, Long)]{
    def digest(a: Double, seed: Long): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }

  implicit def stringHashable128 = new Hashable[String, (Long, Long)]{
    def digest(a: String, seed: Long): (Long, Long) @@ HashCode =
      HashCode(MurmurHash(seed)(a))
  }
}


/**
 * HashCode Converter type class for converting between types (e.g. Long to Int)
 */
trait HashCodeConverter[A, B] extends HashingTags{
  def convert(hc: A @@ HashCode): Seq[B @@ HashCode]

  def convertSeq(hcs: Stream[A @@ HashCode]): Iterable[B @@ HashCode]=
    new Iterable[B @@ HashCode]{
      def iterator = new Iterator[B @@ HashCode]{
        private var cur = hcs

        private val buf: mutable.Queue[B @@ HashCode] = mutable.Queue.empty

        def hasNext = Tags.Disjunction(buf.nonEmpty) |+| Tags.Disjunction(cur.nonEmpty)

        def next = {
          if(buf.isEmpty){
            convert(cur.head) foreach {e => buf enqueue e}
            cur = cur.tail
          }

          buf dequeue
        }
      }
    }

}

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

object hashable
extends HashableInstances
with HashableFunctions
with HashCodeConverterInstances
with HashingTags




