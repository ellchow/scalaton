package scalaton.aggregate.hashed

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._

import scala.collection.BitSet
import com.googlecode.javaewah.{EWAHCompressedBitmap => CompressedBitSet}

// trait BloomFilterConfig[A,H1] extends HashModdedCollectionConfig[A,H1]
// trait StandardBloomFilterConfig[A,H1] extends BloomFilterConfig[A,H1]
// sealed trait DenseStandardBloomFilterConfig[A,H1] extends StandardBloomFilterConfig[A,H1]
// sealed trait SparseStandardBloomFilterConfig[A,H1] extends StandardBloomFilterConfig[A,H1]


// trait BloomFilter[A,H1,D,C <: BloomFilterConfig[A,H1]]
trait BloomFilter[A,H1,D]
extends HashModdedCollection[A,H1]
with InsertsElement[A,H1,Int,D]
with ChecksMembership[A,H1,Int,D]
with Sized[A,H1,Int,D]

trait StandardBloomFilter[A,H1,D] extends BloomFilter[A,H1,D] with Monoid[D] with Equal[D]{
  def add(d: D, a: A)(implicit h: H, hconv: HC): D =
    addToBitSet(d, hashItem(a))

  def contains(d: D, a: A)(implicit h: H, hconv: HC): Boolean =
    hasAllBits(d, hashItem(a))

  /**
   * MLE of number of elements inserted given t bits turned on.
   * NOTE: If bloom filter is full, -1 returned.
   * http://www.softnet.tuc.gr/~papapetrou/publications/Bloomfilters-DAPD.pdf
   **/
  def cardinality(d: D): Long = {
    val t = sizeOfBitSet(d)
    if(t >= width)
      -1L
    else{
      val (m,k) = (width.toDouble, numHashes.toDouble)
      math.round(math.log(1 - t.toDouble / m) / (k * math.log(1 - 1 / m))).toLong
    }
  }

  def sizeOfBitSet(d: D): Int

  def hasAllBits(d: D, bits: Iterable[Int @@ HashCode]): Boolean

  def addToBitSet(d: D, bits: Iterable[Int @@ HashCode]): D

}

abstract class DenseStandardBloomFilter[A,H1,T] extends StandardBloomFilter[A,H1,BitSet @@ T]{

  def tag(b: BitSet) = Tag[BitSet,T](b)

  def hasAllBits(d: BitSet @@ T, bits: Iterable[Int @@ HashCode]): Boolean =
    !(bits exists ( b => !d.contains(b) ))

  def addToBitSet(d: BitSet @@ T, bits: Iterable[Int @@ HashCode]): BitSet  @@ T =
    tag(d ++ bits)

  def sizeOfBitSet(d: BitSet @@ T): Int = d size

  def equal(d1: BitSet @@ T, d2: BitSet @@ T ): Boolean =
    d1 == d2

  val zero: BitSet @@ T = tag(BitSet.empty)

  def append(d1: BitSet @@ T, d2: => BitSet @@ T ) =
    tag(d1 ++ d2)

}

abstract class SparseStandardBloomFilter[A,H1,T] extends StandardBloomFilter[A,H1,CompressedBitSet @@ T]{

  def tag(b: CompressedBitSet) = Tag[CompressedBitSet,T](b)

  def hasAllBits(d: CompressedBitSet @@ T, bits: Iterable[Int @@ HashCode]): Boolean = {
    val b = bitSetFromIterable(bits)
    b equals (d and b)
  }

  def addToBitSet(d: CompressedBitSet @@ T, bits: Iterable[Int @@ HashCode]): CompressedBitSet @@ T =
    tag(d or bitSetFromIterable(bits))

  def bitSetFromIterable(bits: Iterable[Int @@ HashCode]): CompressedBitSet @@ T =
    tag(CompressedBitSet.bitmapOf((bits.toSeq : Seq[Int]).sorted : _*))

  def sizeOfBitSet(d: CompressedBitSet @@ T): Int = d cardinality

  def equal(d1: CompressedBitSet @@ T, d2: CompressedBitSet @@ T ): Boolean =
    d1 equals d2

  val zero: CompressedBitSet @@ T = tag(new CompressedBitSet)

  def append(d1: CompressedBitSet @@ T, d2: => CompressedBitSet @@ T ) =
    tag(d1 or d2)

}

object bloomfilter{
  object sbf extends InsertsElementFunction
                  with ChecksMembershipFunction
                  with SizedFunction{

    /** http://en.wikipedia.org/wiki/Bloom_filter#Probability_of_false_positives **/
    def optimalNumHashes(numItems: Int, width: Int): Int = {
      require(numItems gt 0, "fpProb must be > 0")
      require(width gt 0, "width must be > 0")
      math.ceil(width / numItems * math.log(2)) toInt
    }
    /** http://en.wikipedia.org/wiki/Bloom_filter#Probability_of_false_positives **/
    def optimalWidth(numItems: Int, fpProb: Double): Int = {
      require((fpProb gte 0.0) && (fpProb lte 1.0), "fpProb must be between 0 and 1")
      math.ceil(-1 * numItems * math.log(fpProb) / math.log(2) / math.log(2)) toInt
    }
    def optimalParameters(numItems: Int, fpProb: Double) = {
      val width = optimalWidth(numItems, fpProb)
      val numHashes = optimalNumHashes(numItems, width)

      (numHashes, width)
    }

    def dense[A,H1,T](params: (Int,Int), s: Long = 0L) =
      new DenseStandardBloomFilter[A,H1,T]{
        val (numHashes, width) = params
        val seed = s
      }


    def sparse[A,H1,T](params: (Int,Int), s: Long = 0L) =
      new SparseStandardBloomFilter[A,H1,T]{
        val (numHashes, width) = params
        val seed = s
      }


  }
}
