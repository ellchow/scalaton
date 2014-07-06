/*
 Copyright 2014 Elliot Chow

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
import scala.collection.BitSet
import com.googlecode.javaewah.{EWAHCompressedBitmap => CompressedBitSet}

trait BloomFilter[A,BS, T <: BloomFilter[A,BS,T]] extends DoubleHashModdedCollection[A] {
  val bits: BS

  protected def apply(bs: BS): T
  protected def sizeOfBitSet(bs: BS): Int
  protected def hasBits(bs: BS, bits: Iterable[Int]): Boolean
  protected def setBits(bs: BS, bits: Iterable[Int]): BS
  def ++ (d2: T): T

  def + (a: A): T = apply(setBits(bits, hashItem(a)))
  def contains(a: A): Boolean = hasBits(bits, hashItem(a))
  /**
    * MLE of number of elements inserted given t bits turned on.
    * NOTE: If bloom filter is full, return None
    * http://www.softnet.tuc.gr/~papapetrou/publications/Bloomfilters-DAPD.pdf
    **/
  def size: Option[Long] = {
    val t = sizeOfBitSet(bits)
    if(t >= width) {
      None
    } else {
      val (m,k) = (width.toDouble, numHashes.toDouble)
      math.round(math.log(1 - t.toDouble / m) / (k * math.log(1 - 1 / m))).toLong.some
    }
  }
}
trait StandardBloomFilterParameters {
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

case class StandardBloomFilter[A](bits: BitSet, numHashes: Int, width: Int, seed: Long = 0)(implicit val hashable: Hashable32[A]) extends BloomFilter[A,BitSet,StandardBloomFilter[A]] {
  def apply(bs: BitSet): StandardBloomFilter[A] = this.copy(bits = bs)
  protected def sizeOfBitSet(x: BitSet) = x.size
  protected def setBits(x: BitSet, b: Iterable[Int]) = x ++ b
  protected def hasBits(x: BitSet, b: Iterable[Int]) = !b.exists(i => !x.contains(i))
  def ++ (x: StandardBloomFilter[A]): StandardBloomFilter[A] = {
    require(isCompatible(x), "incompatible bloom filters")
    apply(bits ++ x.bits)
  }

}

case class SparseBloomFilter[A](bits: CompressedBitSet, numHashes: Int, width: Int, seed: Long = 0)(implicit val hashable: Hashable32[A]) extends BloomFilter[A,CompressedBitSet,SparseBloomFilter[A]] {
  protected def apply(bs: CompressedBitSet): SparseBloomFilter[A] = this.copy(bits = bs)
  protected def sizeOfBitSet(x: CompressedBitSet) = bits.cardinality
  protected def setBits(x: CompressedBitSet, b: Iterable[Int]) = bits or bitSetFromIterable(b)
  protected def hasBits(x: CompressedBitSet, b: Iterable[Int]) = bitSetFromIterable(b).equals(bits)

  def ++ (x: SparseBloomFilter[A]): SparseBloomFilter[A] = {
    require(isCompatible(x), "incompatible bloom filters")
    apply(bits or x.bits)
  }

  private def bitSetFromIterable(bits: Iterable[Int]) =
    CompressedBitSet.bitmapOf((bits.toSeq : Seq[Int]).sorted : _*)
}
