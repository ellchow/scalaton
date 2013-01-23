package scalaton.aggregate.hashed

import scala.language.higherKinds
import scala.language.postfixOps

import scala.collection.{SortedSet, BitSet}

import scalaz.{BloomFilter => _, _}
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._


/**
 * Bloom filter
 *
 * http://en.wikipedia.org/wiki/Bloom_filter
 */
trait BloomFilter[A,B,F]
extends HashedCollection[A,B,Int,F]
with MakesSingleton[A,B,Int,F]
with SetLike[A,B,Int,F]
with MapLike[A,B,Int,Boolean,Boolean,F]
with Sized[F] {

  val width: Int

  /** could possibly use double hashing **/
  override def hashItem(item: A)(implicit h: Hashable[A, B],
                                 hconv: HashCodeConverter[B, Int]): Iterable[Int @@ HashCode] = {
    // val hcs = super.hashItem(item)(h,hconv) take 2 toSeq
    // (0 until numHashes) map { i => HashCode(math.abs(hcs(0) + i * hcs(1) + i * i).toInt % width) }
    super.hashItem(item)(h,hconv) map { _ % width |> HashCode}
  }
}



/**
 * Standard Bloom Filter
 **/
sealed trait SBF

trait StandardBloomFilter[A,B]
extends BloomFilter[A,B,BitSet @@ SBF]
with Equal[BitSet @@ SBF] {

  def toBitSet(iter: Iterable[Int @@ HashCode]) = BitSet(iter.toSeq : _*)

  def contains(bits: BitSet @@ SBF, item: A)(implicit h: Hashable[A, B],
                                             hconv: HashCodeConverter[B, Int]): Boolean = {
    val itemBits = toBitSet(hashItem(item))

    (bits & itemBits) == itemBits
  }

  def get(bits: BitSet @@ SBF, item: A)
         (implicit v: Value[Boolean,Boolean],
          h: Hashable[A, B],
          hconv: HashCodeConverter[B, Int]): Boolean =
    contains(bits, item)(h,hconv)

  def insert(bits: BitSet @@ SBF, item: A)(implicit h: Hashable[A, B],
                                           hconv: HashCodeConverter[B, Int]): BitSet @@ SBF =
    Tag[BitSet, SBF](bits ++ toBitSet(hashItem(item)))

  /**
   * MLE of number of elements inserted given t bits turned on.
   * NOTE: If bloom filter is full, -1 returned.
   * http://www.softnet.tuc.gr/~papapetrou/publications/Bloomfilters-DAPD.pdf
  **/
  def cardinality(bits: BitSet @@ SBF):Long = {
    val t = bits.size
    if(t >= width)
      -1L
    else{
      val (m,k) = (width.toDouble, numHashes.toDouble)
      math.round(math.log(1 - t.toDouble / m) / (k * math.log(1 - 1 / m))).toLong
    }
  }
}


object bloomfilter
extends HashedCollectionFunctions
with MakesSingletonFunctions
with SetLikeFunctions
with MapLikeFunctions
with SizedFunctions{

  object StandardBloomFilter{

    val empty: BitSet @@ SBF = Tag[BitSet, SBF](BitSet.empty)

    def apply[A,B](params: (Int, Int), s: Long = 0L) = new StandardBloomFilter[A,B]{

      val (numHashes, width) = params

      val seed: Long = s

      val zero: BitSet @@ SBF = empty

      def equal(sbf1: BitSet @@ SBF, sbf2: BitSet @@ SBF): Boolean =
        sbf1 == sbf2

      def append(sbf1: BitSet @@ SBF, sbf2: => BitSet @@ SBF): BitSet @@ SBF =
        Tag[BitSet, SBF](sbf1 ++ sbf2)

    }

    /** http://en.wikipedia.org/wiki/Bloom_filter#Probability_of_false_positives **/
    def optimalNumHashes(numItems: Int, width: Int): Int =
      math.ceil(width / numItems * math.log(2)).toInt

    /** http://en.wikipedia.org/wiki/Bloom_filter#Probability_of_false_positives **/
    def optimalWidth(numItems: Int, fpProb: Double): Int =
      math.ceil(-1 * numItems * math.log(fpProb) / math.log(2) / math.log(2)).toInt

    def optimalParameters(numItems: Int, fpProb: Double) = {
      val width = optimalWidth(numItems, fpProb)
      val numHashes = optimalNumHashes(numItems, width)

      (numHashes, width)
    }

  }
}




