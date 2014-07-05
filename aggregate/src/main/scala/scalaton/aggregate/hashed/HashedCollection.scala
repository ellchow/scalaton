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

// package scalaton.aggregate.hashed

// import scalaz._
// import Scalaz._

// import scalaton.util._
// import scalaton.util.hashing._
// import scalaton.util.hashing32.Bits32

// /** Collection where keys inserted are hashed **/
// trait HashedCollectionModule{

//   type HashModdedCollectionData[A] = (A, Int, Int, Long)

//   trait HashedCollection[H2,D]{

//     def numHashes(d: D): Int
//     def seed(d: D): Long

//     def isCompatible(d1: D, d2: D) = (numHashes(d1) === numHashes(d2)) && (seed(d1) === seed(d2))

//     /** Compute hash of an item **/
//     def hashItem[A,H1](d: D, item: A)(implicit hashable: Hashable[A, H1], hconverter: HashCodeConverter[H1, H2]): Iterable[H2] =
//       hconverter.convertSeq(multiHash(item, seed(d))).take(numHashes(d))
//   }

//   trait HashModdedCollectionDataFunctions[A]{
//     def numHashes(d: HashModdedCollectionData[A]): Int= d._2
//     def width(d: HashModdedCollectionData[A]): Int = d._3
//     def seed(d: HashModdedCollectionData[A]): Long = d._4
//   }

//   trait HashModdedCollection[D] extends HashedCollection[Bits32,D]{
//     def width(d: D): Int

//     override def isCompatible(d1: D, d2: D) = super.isCompatible(d1, d2) && (width(d1) === width(d2))

//     override def hashItem[A,H1](d: D, item: A)(implicit hashable: Hashable[A, H1], hconverter: HashCodeConverter[H1, Bits32]): Iterable[Bits32] =
//       super.hashItem(d, item).map(x => x % width(d))
//   }

//   trait DoubleHashModdedCollection[D] extends HashModdedCollection[D]{

//     private def doubleHashStream(a: Bits32, b: Bits32, i: Int, w: Int): Stream[Bits32] =
//       Stream.cons(math.abs((a + i * b + i * i) % w) , doubleHashStream(a, b, i + 1, w))

//     override def hashItem[A,H1](d: D, item: A)(implicit h: Hashable[A, H1],
//                                                hconv: HashCodeConverter[H1, Bits32]): Iterable[Bits32] = {
//       val base = hconv.convertSeq(multiHash(item, seed(d))) take 2 toSeq
//       val a = base(0)
//       val b = base(1)

//       doubleHashStream(a, b, 1, width(d)) take numHashes(d)
//     }
//   }

//   implicit class HashedCollectionOps[H2,D](val d: D)(implicit hc: HashedCollection[H2,D]){
//     def numHashes: Int = hc.numHashes(d)
//     def seed: Long = hc.seed(d)
//   }

// }
