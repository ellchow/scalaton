package scalaton.hashed

import scala.language.higherKinds
import scala.language.postfixOps

import scala.collection.{SortedSet, BitSet}

import scalaz.{BloomFilter => _, _}
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._

trait BloomFilter[A,B,G[_],F <: G[_]]
extends MakesSingleton[A,B,Int,F]
with Contains[A,B,Int,F]{

  val width: Int

  override def hashItem(item: A)(implicit h: Hashable[A, B],
                                 hconv: HashCodeConverter[B, Int]): Iterable[Int @@ HashCode] =
    super.hashItem(item)(h,hconv) map { _ % width |> HashCode}
}

sealed trait SBF

trait StandardBloomFilter[A,B] extends BloomFilter[A,B,SortedSet,BitSet @@ SBF] with Monoid[BitSet @@ SBF] with Equal[BitSet @@ SBF] {

  def toBitSet(iter: Iterable[Int @@ HashCode]) = BitSet(iter.toSeq : _*)

  def contains(bits: BitSet @@ SBF, item: A)(implicit h: Hashable[A, B],
                                             hconv: HashCodeConverter[B, Int]): Boolean = {
    val itemBits = toBitSet(hashItem(item))

    (bits & itemBits) == itemBits
  }

  def insert(bits: BitSet @@ SBF, item: A)(implicit h: Hashable[A, B],
                                           hconv: HashCodeConverter[B, Int]): BitSet @@ SBF =
    Tag[BitSet, SBF](bits ++ toBitSet(hashItem(item)))

  def singleton(item: A)(implicit h: Hashable[A, B],
                         hconv: HashCodeConverter[B, Int]) =
    insert(zero, item)
}


object bloomfilter
extends InsertsFunctions
with MakesSingletonFunctions
with ContainsFunctions
with SizesFunctions{

  object StandardBloomFilter{

    val empty: BitSet @@ SBF = Tag[BitSet, SBF](BitSet.empty)

    def apply[A,B](k: Int, w: Int, s: Long) = new StandardBloomFilter[A,B]{
      val numHashes: Int = k

      val width: Int = w

      val seed: Long = s

      val zero: BitSet @@ SBF = empty

      def equal(sbf1: BitSet @@ SBF, sbf2: BitSet @@ SBF): Boolean =
        sbf1 == sbf2

      def append(sbf1: BitSet @@ SBF, sbf2: => BitSet @@ SBF): BitSet @@ SBF =
        Tag[BitSet, SBF](sbf1 ++ sbf2)
    }
  }
}




