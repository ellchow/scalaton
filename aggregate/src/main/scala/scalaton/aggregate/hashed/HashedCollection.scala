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

/** Collection where keys inserted are hashed **/
trait HashedCollectionModule{

  type HashedCollectionData[A] = (A, Int, Int, Long)

  trait HashedCollection[H2,D]{

    def numHashes(d: D): Int
    def width(d: D): Int
    def seed(d: D): Long
    def empty(h: Int, w: Int, s: Long): D

    def isCompatible(d1: D, d2: D) =
      (numHashes(d1) === numHashes(d2)) && (width(d1) === width(d2)) && (seed(d1) === seed(d2))

    /** Compute hash of an item **/
    def hashItem[A,H1](d: D, item: A)(implicit hashable: Hashable[A, H1], hconverter: HashCodeConverter[H1, H2]): Iterable[H2] =
      hconverter.convertSeq(multiHash(item, seed(d))).take(numHashes(d))
  }

  trait HashedCollectionDataFunctions[A]{
    def numHashes(d: HashedCollectionData[A]): Int= d._2
    def width(d: HashedCollectionData[A]): Int = d._3
    def seed(d: HashedCollectionData[A]): Long = d._4
  }

  trait HashModdedCollection[D] extends HashedCollection[Bits32,D]{
    override def hashItem[A,H1](d: D, item: A)(implicit hashable: Hashable[A, H1], hconverter: HashCodeConverter[H1, Bits32]): Iterable[Bits32] =
      super.hashItem(d, item).map(x => x % width(d))
  }

  trait DoubleHashModdedCollection[D] extends HashModdedCollection[D]{

    private def doubleHashStream(a: Bits32, b: Bits32, i: Int, w: Int): Stream[Bits32] =
      Stream.cons(math.abs((a + i * b + i * i) % w) , doubleHashStream(a, b, i + 1, w))

    override def hashItem[A,H1](d: D, item: A)(implicit h: Hashable[A, H1],
                                               hconv: HashCodeConverter[H1, Bits32]): Iterable[Bits32] = {
      val base = hconv.convertSeq(multiHash(item, seed(d))) take 2 toSeq
      val a = base(0)
      val b = base(1)

      doubleHashStream(a, b, 1, width(d)) take numHashes(d)
    }
  }

  implicit class HashedCollectionOps[H2,D](val d: D)(implicit hc: HashedCollection[H2,D]){
    def numHashes: Int = hc.numHashes(d)
    def width: Int = hc.width(d)
    def seed: Long = hc.seed(d)
  }

}



// trait HashedCollection[A,H1,H2]{
//   type H = Hashable[A,H1]
//   type HC = HashCodeConverter[H1,H2]

//   /** Number of hashes used in this collection **/
//   val numHashes: Int

//   /** Seed of hashing function **/
//   val seed: Long

//   /** Compute hash of an item **/
//   def hashItem(item: A)(implicit h: Hashable[A, H1],
//                         hconv: HashCodeConverter[H1, H2]): Iterable[H2 @@ HashCode] =
//     hconv.convertSeq(multiHash(item, seed)) take numHashes

// }

// trait HashModdedCollection[A,H1] extends HashedCollection[A,H1,Bits32]{
//   val width: Int

//   override def hashItem(item: A)(implicit h: Hashable[A, H1],
//                                  hconv: HashCodeConverter[H1, Bits32]): Iterable[Bits32 @@ HashCode] =
//     super.hashItem(item) map { _ % width |> HashCode}
// }

// /** Uses extended double hashing to improve runtime performance (http://www.eecs.harvard.edu/~michaelm/CS223/lesshash.pdf) **/
// trait DoubleHashModdedCollection[A,H1] extends HashModdedCollection[A,H1]{

//   private def doubleHashStream(a: Bits32, b: Bits32, i: Int): Stream[Bits32 @@ HashCode] =
//     Stream.cons(HashCode(math.abs((a + i * b + i * i) % width)) , doubleHashStream(a, b, i + 1))

//   override def hashItem(item: A)(implicit h: Hashable[A, H1],
//                                  hconv: HashCodeConverter[H1, Bits32]): Iterable[Bits32 @@ HashCode] = {
//     val base = hconv.convertSeq(multiHash(item, seed)) take 2 toSeq
//     val a = base(0)
//     val b = base(1)

//     doubleHashStream(a, b, 1) take numHashes
//   }

// }


// trait HashedCollectionOperations[A,H1]{
//   type H = Hashable[A,H1]
//   type HC = HashCodeConverter[H1,Int]
// }

// trait InsertsElement[A,H1,H2,D] extends HashedCollectionOperations[A,H1]{
//   def insert(d: D, a: A)(implicit h: H, hconv: HC): D
// }

// trait InsertsElementFunction{
//   def insert[A,H1,H2,D](d: D, a: A)(implicit i: InsertsElement[A,H1,H2,D], h: Hashable[A,H1], hconv: HashCodeConverter[H1,Int]): D =
//     i.insert(d, a)
// }

// trait ChecksMembership[A,H1,H2,D] extends HashedCollectionOperations[A,H1]{
//   def contains(d: D, a: A)(implicit h: H, hconv: HC): Boolean
// }

// trait ChecksMembershipFunction{
//   def contains[A,H1,H2,D](d: D, a: A)(implicit c: ChecksMembership[A,H1,H2,D], h: Hashable[A,H1], hconv: HashCodeConverter[H1,Int]): Boolean =
//     c.contains(d,a)
// }

// trait Sized[A,H1,H2,D] extends HashedCollectionOperations[A,H1]{
//   def cardinality(d: D): Long
// }

// trait SizedFunction{
//   def cardinality[A,H1,H2,D](d: D)(implicit s: Sized[A,H1,H2,D], h: Hashable[A,H1], hconv: HashCodeConverter[H1,Int]): Long = s.cardinality(d)
// }

// trait UpdatesElementValue[A,H1,H2,D,V1] extends HashedCollectionOperations[A,H1]{
//   def update(d: D, a: A, v1: V1)(implicit h: H, hconv: HC): D
// }

// trait UpdatesElementValueFunction{
//   def update[A,H1,H2,D,V1](d: D, a: A, v1: V1)(implicit u: UpdatesElementValue[A,H1,H2,D,V1], h: Hashable[A,H1], hconv: HashCodeConverter[H1,Int]): D =
//     u.update(d,a,v1)
// }

// trait LooksUpElementValue[A,H1,H2,D,V2] extends HashedCollectionOperations[A,H1]{
//   def lookup(d: D, a: A)(implicit h: H, hconv: HC): V2
// }

// trait LooksUpElementValueFunction{
//   def lookup[A,H1,H2,D,V2](d: D, a: A)(implicit l: LooksUpElementValue[A,H1,H2,D,V2], h: Hashable[A,H1], hconv: HashCodeConverter[H1,Int]): V2 =
//     l.lookup(d,a)
// }

// trait MakesSingletonFunction{
//   def singleton[A,H1,H2,D,T](a: A @@ T)(implicit i: InsertsElement[A,H1,H2,D @@ T], m: Monoid[D @@ T], h: Hashable[A,H1], hconv: HashCodeConverter[H1,Int]): D @@ T =
//     i.insert(m.zero, a)

//   def singleton[A,H1,H2,D,T,V1](a: A @@ T, v1: V1)(implicit u: UpdatesElementValue[A,H1,H2,D @@ T,V1], m: Monoid[D @@ T], h: Hashable[A,H1], hconv: HashCodeConverter[H1,Int]): D @@ T=
//     u.update(m.zero, a, v1)
// }

// object hcollection
// extends InsertsElementFunction
// with ChecksMembershipFunction
// with SizedFunction
// with MakesSingletonFunction
// with UpdatesElementValueFunction
// with LooksUpElementValueFunction
