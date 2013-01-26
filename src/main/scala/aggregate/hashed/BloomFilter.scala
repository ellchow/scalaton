package scalaton.aggregate.hashed

import scala.language.higherKinds
import scala.language.postfixOps

import scala.collection.{SortedSet, BitSet}

import scalaz._
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
with Sized[F] {

  val width: Int

  /** could possibly use double hashing **/
  override def hashItem(item: A)(implicit h: Hashable[A, B],
                                 hconv: HashCodeConverter[B, Int]): Iterable[Int @@ HashCode] = {
    // val hcs = super.hashItem(item)(h,hconv) take 2 toSeq
    // (0 until numHashes) map { i => HashCode(math.abs(hcs(0) + i * hcs(1) + i * i).toInt % width) }
    super.hashItem(item) map { _ % width |> HashCode}
  }
}

sealed trait BF

/**
 * Standard Bloom Filter
 **/
trait StandardBloomFilter[A,B]
extends BloomFilter[A,B,BitSet @@ BF]
with Equal[BitSet @@ BF] {

  def toBitSet(iter: Iterable[Int @@ HashCode]) = BitSet(iter.toSeq : _*)

  def contains(bits: BitSet @@ BF, item: A)(implicit h: Hashable[A, B],
                                             hconv: HashCodeConverter[B, Int]): Boolean = {
    val itemBits = (hashItem _ map toBitSet)(item)

    (bits & itemBits) == itemBits
  }


  def insert(bits: BitSet @@ BF, item: A)(implicit h: Hashable[A, B],
                                          hconv: HashCodeConverter[B, Int]): BitSet @@ BF =
    Tag[BitSet, BF](bits ++ (hashItem _ map toBitSet)(item))

  /**
   * MLE of number of elements inserted given t bits turned on.
   * NOTE: If bloom filter is full, -1 returned.
   * http://www.softnet.tuc.gr/~papapetrou/publications/Bloomfilters-DAPD.pdf
  **/
  def cardinality(bits: BitSet @@ BF):Long = {
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

  type SBF = BitSet @@ BF

  object StandardBloomFilter{

    val empty: SBF = Tag[BitSet, BF](BitSet.empty)

    def apply[A,B](params: (Int, Int), s: Long = 0L) = new StandardBloomFilter[A,B]{

      val (numHashes, width) = params

      val seed: Long = s

      val zero: SBF = empty

      def equal(sbf1: SBF, sbf2: SBF): Boolean =
        sbf1 == sbf2

      def append(sbf1: SBF, sbf2: => SBF): SBF =
        Tag[BitSet, BF](sbf1 ++ sbf2)

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




