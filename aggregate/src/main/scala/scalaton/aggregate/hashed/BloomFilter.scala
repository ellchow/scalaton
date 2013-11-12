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


/** Bloom filter data structure that allows for checking membership **/
trait BloomFilterT[A,H1,D]
extends DoubleHashModdedCollection[A,H1]
with InsertsElement[A,H1,Bits32,D]
with ChecksMembership[A,H1,Bits32,D]
with Sized[A,H1,Bits32,D]

/** standard bloom filter backed by bitset **/
trait StandardBloomFilterT[A,H1,D] extends BloomFilterT[A,H1,D] with Monoid[D] with Equal[D]{
  def insert(d: D, a: A)(implicit h: H, hconv: HC): D =
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

  /** get number of bits set to 1 **/
  protected def sizeOfBitSet(d: D): Int

  /** check if given bits are contained in the bit set **/
  protected def hasAllBits(d: D, bits: Iterable[Int @@ HashCode]): Boolean

  /** set bits to 1 in the bit set **/
  protected def addToBitSet(d: D, bits: Iterable[Int @@ HashCode]): D

}

/** standard bloom filter backed by immutable bitset **/
abstract class DenseStandardBloomFilterT[A,H1,T] extends StandardBloomFilterT[A,H1,BitSet @@ T]{

  def equal(d1: BitSet @@ T, d2: BitSet @@ T ): Boolean =
    d1 == d2

  val zero: BitSet @@ T = tag(BitSet.empty)

  def append(d1: BitSet @@ T, d2: => BitSet @@ T ) =
    tag(d1 ++ d2)

  protected def tag(b: BitSet) = Tag[BitSet,T](b)

  protected def hasAllBits(d: BitSet @@ T, bits: Iterable[Int @@ HashCode]): Boolean =
    !(bits exists ( b => !d.contains(b) ))

  protected def addToBitSet(d: BitSet @@ T, bits: Iterable[Int @@ HashCode]): BitSet  @@ T =
    tag(d ++ bits)

  protected def sizeOfBitSet(d: BitSet @@ T): Int = d size
}

trait StandardBloomFilterParameterEstimate{
  /** http://en.wikipedia.org/wiki/Bloom_filter#Probability_of_false_positives **/
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

object bloomfilter{
  object sbf
  extends StandardBloomFilterParameterEstimate {
    def apply[A,H1,T](params: (Int,Int), s: Long = 0L) =
      new DenseStandardBloomFilterT[A,H1,T]{
        val (numHashes, width) = params
        val seed = s
      }
  }
}
