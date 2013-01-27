/*

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

    def dense[A,B](params: (Int, Int), s: Long = 0L): StandardBloomFilter[A,B,DenseStandardBloomFilter.DSBF] =
      DenseStandardBloomFilter(params, s)

    def sparse[A,B](params: (Int, Int), s: Long = 0L): StandardBloomFilter[A,B,SparseStandardBloomFilter.SSBF] =
      SparseStandardBloomFilter(params, s)

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
          union(sbf, (hashItem _ map toBitSet)(item))

        def union(sbf1: DSBF, sbf2: => DSBF): DSBF =
          DSBF(sbf1 ++ sbf2)

        def intersect(sbf1: DSBF, sbf2: DSBF): DSBF = DSBF(sbf1 & sbf2)

        def size(sbf: DSBF) = sbf.size

        private def toBitSet(iter: Iterable[Int @@ HashCode]) = DSBF(BitSet(iter.toSeq : _*))
      }
  }


  sealed trait SparseStandardBloomFilter
  object SparseStandardBloomFilter{
    type SSBF = CompressedBitSet @@ SparseStandardBloomFilter

    def SSBF(b: CompressedBitSet) = Tag[CompressedBitSet, SparseStandardBloomFilter](b)

    val empty = SSBF(new CompressedBitSet)

    def apply[A,B](params: (Int, Int), s: Long = 0L) =
      new StandardBloomFilter[A,B,SSBF]{
        val (numHashes, width) = params
        val seed: Long = s

        val zero = empty

        def equal(sbf1: SSBF, sbf2: SSBF): Boolean =
          sbf1 equals sbf2

        def insert(sbf: SSBF, item: A)(implicit h: Hashable[A, B],
                                       hconv: HashCodeConverter[B, Int]): SSBF =
          union(sbf, SSBF(CompressedBitSet.bitmapOf((hashItem(item).toSeq : Seq[Int]).sorted : _*)))

        def union(sbf1: SSBF, sbf2: => SSBF): SSBF =
          SSBF(sbf1 or sbf2)

        def intersect(sbf1: SSBF, sbf2: SSBF): SSBF = SSBF(sbf1 and sbf2)

        def size(sbf: SSBF) = sbf.cardinality
      }
  }

}
*/
