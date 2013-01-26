package scalaton.aggregate.hashed

import scala.collection.BitSet

import com.googlecode.javaewah.{EWAHCompressedBitmap => CompressedBitSet}

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._



trait StandardBloomFilter[A,B,F]
extends BloomFilter[A,B,F]
with Equal[F] {

  def union(cells1: F, cells2: => F): F

  def intersect(cells1: F, cells2: F): F

  def size(cells: F): Int

  def append(cells1: F, cells2: => F): F = union(cells1, cells2)

  def contains(cells: F, item: A)(implicit h: Hashable[A, B],
                                  hconv: HashCodeConverter[B, Int]): Boolean = {
    val hashedItem = insert(zero, item)
    equal(intersect(cells, hashedItem), hashedItem)
  }

  /**
   * MLE of number of elements inserted given t bits turned on.
   * NOTE: If bloom filter is full, -1 returned.
   * http://www.softnet.tuc.gr/~papapetrou/publications/Bloomfilters-DAPD.pdf
   **/
  def cardinality(cells: F): Long = {
    val t = size(cells)
    if(t >= width)
      -1L
    else{
      val (m,k) = (width.toDouble, numHashes.toDouble)
      math.round(math.log(1 - t.toDouble / m) / (k * math.log(1 - 1 / m))).toLong
    }
  }
}




trait StandardBloomFilterInstances{

  object StandardBloomFilter{
    def apply[A,B](params: (Int, Int), s: Long = 0L): StandardBloomFilter[A,B,DenseStandardBloomFilter.DSBF] =
      DenseStandardBloomFilter(params, s)

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

  sealed trait DenseStandardBloomFilter
  object DenseStandardBloomFilter{
    type DSBF = BitSet @@ DenseStandardBloomFilter

    def DSBF(b: BitSet) = Tag[BitSet, DenseStandardBloomFilter](b)

    val empty = DSBF(BitSet.empty)

    def apply[A,B](params: (Int, Int), s: Long = 0L) =
      new StandardBloomFilter[A,B,DSBF]{
        val (numHashes, width) = params
        val seed: Long = s

        val zero = empty

        def equal(sbf1: DSBF, sbf2: DSBF): Boolean =
          sbf1 == sbf2

        def insert(sbf: DSBF, item: A)(implicit h: Hashable[A, B],
                                       hconv: HashCodeConverter[B, Int]): DSBF =
          append(sbf, (hashItem _ map toBitSet)(item))

        def union(sbf1: DSBF, sbf2: => DSBF): DSBF =
          DSBF(sbf1 ++ sbf2)

        def intersect(sbf1: DSBF, sbf2: DSBF): DSBF = DSBF(sbf1 & sbf2)

        def size(sbf: DSBF) = sbf.size

        private def toBitSet(iter: Iterable[Int @@ HashCode]) = DSBF(BitSet(iter.toSeq : _*))
      }
  }

}
