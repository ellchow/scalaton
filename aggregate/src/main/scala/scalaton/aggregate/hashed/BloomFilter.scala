/*
 Copyright 2013 Elliot Chow

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

package scalaton.aggregate.hashed

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.hashing._
import scalaton.util.hashing32.Bits32

import scala.collection.BitSet
import com.googlecode.javaewah.{EWAHCompressedBitmap => CompressedBitSet}

trait BloomFilterModule extends HashedCollectionModule{
  trait BloomFilter[D] extends DoubleHashModdedCollection[D]{
    def insert[A,H1](d: D, a: A)(implicit h: Hashable[A,H1], hconv: HashCodeConverter[H1,Bits32]): D

    def contains[A,H1](d: D, a: A)(implicit h: Hashable[A,H1], hconv: HashCodeConverter[H1,Bits32]): Boolean

    def merge(d1: D, d2: D): D

    def size(d: D): Option[Long]

    def sizeOfBitSet(d: D): Int

    def hasBits(d: D, bits: Iterable[Bits32]): Boolean

    def setBits(d: D, bits: Iterable[Bits32]): D

    def insert[A,H1](d: D, as: Iterable[A])(implicit h: Hashable[A,H1], hconv: HashCodeConverter[H1,Bits32]): D = as.foldLeft(d)((dd, a) => insert(dd, a))

    def fromData[A,H1](h: Int, w: Int, s: Long)(as: Iterable[A])(implicit ha: Hashable[A,H1], hconv: HashCodeConverter[H1,Bits32]) = insert(empty(h,w,s), as)
  }

  trait StandardBloomFilter[D] extends BloomFilter[D]{

    def insert[A,H1](d: D, a: A)(implicit h: Hashable[A,H1], hconv: HashCodeConverter[H1,Bits32]): D =
      setBits(d, hashItem(d,a))

    def contains[A,H1](d: D, a: A)(implicit h: Hashable[A,H1], hconv: HashCodeConverter[H1,Bits32]): Boolean =
      hasBits(d, hashItem(d,a))

    /**
     * MLE of number of elements inserted given t bits turned on.
     * NOTE: If bloom filter is full, return None
     * http://www.softnet.tuc.gr/~papapetrou/publications/Bloomfilters-DAPD.pdf
     **/
    def size(d: D): Option[Long] = {
      val t = sizeOfBitSet(d)
      if(t >= width(d))
        none[Long]
      else{
        val (m,k) = (width(d).toDouble, numHashes(d).toDouble)
        math.round(math.log(1 - t.toDouble / m) / (k * math.log(1 - 1 / m))).toLong.some
      }
    }

    def optimalNumHashes(numItems: Int, width: Int): Int = {
      require(numItems gt 0, "numItems must be > 0")
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

  type DenseStandardBloomFilterData = HashedCollectionData[BitSet]

  trait DenseStandardBloomFilter extends StandardBloomFilter[DenseStandardBloomFilterData] with HashedCollectionDataFunctions[BitSet]{
    type D = DenseStandardBloomFilterData

    def empty(h: Int, w: Int, s: Long) = (BitSet.empty, h, w, s)

    def sizeOfBitSet(d: D): Int = d._1.size

    def hasBits(d: D, bits: Iterable[Bits32]): Boolean =
      !bits.exists( b => !d._1.contains(b) )

    def setBits(d: D, bits: Iterable[Bits32]): D =
      d.copy(_1 = d._1 ++ bits)

    def merge(d1: D, d2: D): D = {
      require(isCompatible(d1,d2))
      d1.copy(_1 = d1._1 ++ d2._1)
    }
  }
  implicit object DenseStandardBloomFilter extends DenseStandardBloomFilter

  type SparseStandardBloomFilterData = HashedCollectionData[CompressedBitSet]

  trait SparseStandardBloomFilter extends StandardBloomFilter[SparseStandardBloomFilterData] with HashedCollectionDataFunctions[CompressedBitSet]{
    type D = SparseStandardBloomFilterData

    def empty(h: Int, w: Int, s: Long) = (new CompressedBitSet, h, w, s)

    def sizeOfBitSet(d: D): Int = d._1.cardinality

    def hasBits(d: D, bits: Iterable[Bits32]): Boolean = {
      val b = bitSetFromIterable(bits)
      b.equals(d._1 and b)
    }

    def setBits(d: D, bits: Iterable[Bits32]): D =
    d.copy(_1 = d._1 or bitSetFromIterable(bits))

    def merge(d1: D, d2: D): D = {
      require(isCompatible(d1,d2))
      d1.copy(_1 = d1._1 or d2._1)
    }

    protected def bitSetFromIterable(bits: Iterable[Bits32]) =
      CompressedBitSet.bitmapOf((bits.toSeq : Seq[Int]).sorted : _*)

  }
  implicit object SparseStandardBloomFilter extends SparseStandardBloomFilter


  object implicits{
    implicit def bloomFilterSemigroup[D : BloomFilter]: Semigroup[D] = new Semigroup[D]{
      def append(d1: D, d2: => D) = implicitly[BloomFilter[D]].merge(d1,d2)
    }

    implicit class BloomFilterOps[D](val d: D)(implicit bf: BloomFilter[D]){
      def insert[A,H1](a: A)(implicit h: Hashable[A,H1], hconv: HashCodeConverter[H1,Bits32]) = bf.insert(d,a)

      def contains[A,H1](a: A)(implicit h: Hashable[A,H1], hconv: HashCodeConverter[H1,Bits32]): Boolean = bf.contains(d,a)

      def merge(d2: D): D = bf.merge(d, d2)

      def size: Option[Long] = bf.size(d)
    }

  }
}

object bloomfilter extends BloomFilterModule


