// /*
//  Copyright 2013 Elliot Chow

//  Licensed under the Apache License, Version 2.0 (the "License")
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at

//  http://www.apache.org/licenses/LICENSE-2.0

//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
// */

// package scalaton.aggregate.hashed.mutable

// import scalaz._
// import Scalaz._

// import scalaton.aggregate.hashed._

// import scalaton.util._
// import scalaton.util.hashing._

// import scala.collection.mutable.BitSet
// import com.googlecode.javaewah.{EWAHCompressedBitmap => CompressedBitSet}

// /** standard bloom filter backed by mutable bitset **/
// abstract class DenseStandardBloomFilterT[A,H1,T] extends StandardBloomFilterT[A,H1,BitSet @@ T]{

//   def tag(b: BitSet) = Tag[BitSet,T](b)

//   def hasAllBits(d: BitSet @@ T, bits: Iterable[Int @@ HashCode]): Boolean =
//     !(bits exists ( b => !d.contains(b) ))

//   def addToBitSet(d: BitSet @@ T, bits: Iterable[Int @@ HashCode]): BitSet  @@ T =
//     tag(d ++= bits)

//   def sizeOfBitSet(d: BitSet @@ T): Int = d size

//   def equal(d1: BitSet @@ T, d2: BitSet @@ T ): Boolean =
//     d1 == d2

//   def zero: BitSet @@ T = tag(BitSet.empty)

//   def append(d1: BitSet @@ T, d2: => BitSet @@ T ) =
//     tag(d1 ++= d2)

// }

// /** standard bloom filter backed by mutable, sparse bitset; good for large filters at the cost of some runtime performance  **/
// abstract class SparseStandardBloomFilterT[A,H1,T] extends StandardBloomFilterT[A,H1,CompressedBitSet @@ T]{

//   def tag(b: CompressedBitSet) = Tag[CompressedBitSet,T](b)

//   def hasAllBits(d: CompressedBitSet @@ T, bits: Iterable[Int @@ HashCode]): Boolean = {
//     val b = bitSetFromIterable(bits)
//     b equals (d and b)
//   }

//   def addToBitSet(d: CompressedBitSet @@ T, bits: Iterable[Int @@ HashCode]): CompressedBitSet @@ T =
//     tag(d or bitSetFromIterable(bits))

//   def bitSetFromIterable(bits: Iterable[Int @@ HashCode]): CompressedBitSet @@ T =
//     tag(CompressedBitSet.bitmapOf((bits.toSeq : Seq[Int]).sorted : _*))

//   def sizeOfBitSet(d: CompressedBitSet @@ T): Int = d cardinality

//   def equal(d1: CompressedBitSet @@ T, d2: CompressedBitSet @@ T ): Boolean =
//     d1 equals d2

//   def zero: CompressedBitSet @@ T = tag(new CompressedBitSet)

//   def append(d1: CompressedBitSet @@ T, d2: => CompressedBitSet @@ T ) =
//     tag(d1 or d2)

// }

// object bloomfilter{
//   object sbf
//   extends StandardBloomFilterParameterEstimate {

//     def dense[A,H1,T](params: (Int,Int), s: Long = 0L) =
//       new DenseStandardBloomFilterT[A,H1,T]{
//         val (numHashes, width) = params
//         val seed = s
//       }

//     def sparse[A,H1,T](params: (Int,Int), s: Long = 0L) =
//       new SparseStandardBloomFilterT[A,H1,T]{
//         val (numHashes, width) = params
//         val seed = s
//       }
//   }
// }
