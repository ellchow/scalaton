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


/** sketch data structure to store and lookup values given a key, generally used for keeping track of frequencies **/
trait SketchModule extends HashedCollectionModule{

  abstract class Sketch[F[_], V, W](implicit monoidV: Monoid[V], skt: SketchTable[F,V])
           extends DoubleHashModdedCollection[F[V]]{

    def update[K,H1](d: F[V], k: K, v: V)(implicit h: Hashable[K,H1], hconv: HashCodeConverter[H1,Bits32]): F[V] = {
      val cells = getCells(d, k)

      val d1 = cells.foldLeft(d){ case (dd, (i, j)) => skt.updateCell(dd, i, j, v) }

      val d2 = skt.updateSize(d1, 1L)

      d2
    }

    def lookup[K,H1](d: F[V], k: K)(implicit h: Hashable[K,H1], hconv: HashCodeConverter[H1,Bits32]): W = {
      val cells = getCells(d, k)

      estimate(cells.map{ case (i, j) => skt.readCell(d, i,j) })
    }

    def merge(d1: F[V], d2: F[V]) = {
      require(isCompatible(d1, d2))

      val cells = for{
        i <- (0 until numHashes(d1)).view
        j <- (0 until width(d1)).view
      } yield (i, j)

      val d3 = cells.foldLeft(d1){ case (dd, (i, j)) => skt.updateCell(dd, i, j, skt.readCell(d2, i, j)) }

      val d4 = skt.updateSize(d3, skt.size(d2))

      d4
    }

    def empty(h: Int, w: Int, s: Long): F[V]

    def estimate(vs: Iterable[V]): W

    def getCells[K,H1](d: F[V], k: K)(implicit h: Hashable[K,H1], hconv: HashCodeConverter[H1,Bits32]): Iterable[(Int, Int)] = (0 to numHashes(d)).view.zip(hashItem(d,k))

    def fromData[K,H1](h: Int, w: Int, s: Long)(counts: Iterable[(K,V)])(implicit ha: Hashable[K,H1], hconv: HashCodeConverter[H1,Bits32]) =
      counts.foldLeft(empty(h,w,s)){ case (dd, (k, v)) => update(dd, k, v) }
  }

  abstract class SketchTable[F[_], V : Monoid]{
    def size(d: F[V]): Long

    def readCell(d: F[V], i: Int, j: Int): V

    def updateCell(d: F[V], i: Int, j: Int, v: V): F[V]

    def updateSize(d: F[V], i: Long): F[V]
  }


  type VectorBackedTable[A] = (Vector[Vector[A]], Long)
  type HCVectorBackedTable[A] = HashModdedCollectionData[VectorBackedTable[A]]

  implicit def denseSketchTable[V : Monoid] = new SketchTable[HCVectorBackedTable, V]{
    def size(d: HCVectorBackedTable[V]) = d._2

    def readCell(d: HCVectorBackedTable[V], i: Int, j: Int) = d._1._1(i)(j)

    def updateCell(d: HCVectorBackedTable[V], i: Int, j: Int, v: V) = {
      def table = d._1._1

      d.copy(_1 = d._1.copy(_1 = table.updated(i, table(i).updated(j, readCell(d,i,j) |+| v))))
    }

    def updateSize(d: HCVectorBackedTable[V], i: Long) = d.copy(_1 = d._1.copy(_2 = d._1._2 + i))

  }

  abstract class HCVectorBackedTableSketch[V : Monoid, W] extends Sketch[HCVectorBackedTable, V, W] with HashModdedCollectionDataFunctions[VectorBackedTable[V]]{
    def empty(h: Int, w: Int, s: Long) = ((Vector.fill(h, w)(implicitly[Monoid[V]].zero), 0L), h, w, s)
  }

  implicit object countminsketch extends HCVectorBackedTableSketch[Long, Long]{
    def estimate(vs: Iterable[Long]): Long = vs.min

    /** delta is certainty having less than eps **/
    def optimalNumHashes(delta: Double) = {
      require((delta gte 0.0) && (delta lte 1.0), "delta must be between 0 and 1")
      math.ceil(math.log(1.0 / (1 - delta))) toInt
    }

    /** eps is max tolerable error **/
    def optimalWidth(eps: Double) = {
      require((eps gte 0.0) && (eps lte 1.0), "eps must be between 0 and 1")
      math.ceil(math.exp(1) / eps) toInt
    }

    def optimalParameters(eps: Double, delta: Double) = (optimalNumHashes(delta), optimalWidth(eps))
  }

  class SketchOps[F[_], V, W](val d: F[V])(implicit monoidV: Monoid[V], skt: SketchTable[F,V], sk: Sketch[F, V, W]){
    def update[K,H1](k: K, v: V)(implicit h: Hashable[K,H1], hconv: HashCodeConverter[H1,Bits32]): F[V] =  sk.update(d, k, v)

    def lookup[K,H1](k: K)(implicit h: Hashable[K,H1], hconv: HashCodeConverter[H1,Bits32]): W = sk.lookup(d, k)

    def merge(d2: F[V]) = sk.merge(d, d2)
  }

  object implicits{
    implicit def sketchSemigroup[F[_], V, W](implicit monoidV: Monoid[V], skt: SketchTable[F,V], sk: Sketch[F, V, W]): Semigroup[F[V]] = new Semigroup[F[V]]{
      def append(d1: F[V], d2: => F[V]) = sk.merge(d1, d2)
    }

    implicit class CountMinSketchOps[F[_], Long](override val d: F[Long])(implicit monoidV: Monoid[Long], skt: SketchTable[F,Long], sk: Sketch[F, Long, Long]) extends SketchOps[F, Long, Long](d)

  }

}

object sketch extends SketchModule

// /** sketch requiring cell values to be monoids **/
// abstract class FrequencySketch[A,H1,D,V1 : Monoid]
// extends DoubleHashModdedCollection[A,H1]
// with UpdatesElementValue[A,H1,Bits32,D,V1]
// with LooksUpElementValue[A,H1,Bits32,D,Long]
// with Sized[A,H1,Bits32,D]{

//   def update(d: D, a: A, v1: V1)(implicit h: H, hconv: HC): D = {
//     val ijs = itemIJs(a)
//     newSize(newData(d, ijs, v1), v1)
//   }

//   def lookup(d: D, a: A)(implicit h: H, hconv: HC): Long = {
//     val ijs = itemIJs(a)

//     estimate(ijs map { case(i,j) => valueToLong(valueAt(d,i,j)) })
//   }

//   /** get cells to update **/
//   def itemIJs(a: A)(implicit h: H, hconv: HC): Iterable[(Int,Int)] =
//     (0 to numHashes).view zip hashItem(a)

//   /** update a cell's value given an input **/
//   def updateValueWith(v: V1, u: V1): V1 = v |+| u

//   /** compute estimate given the values extracted from each cell **/
//   protected def estimate(cs: Iterable[Long]): Long

//   /** extract long from value stored in cell **/
//   protected def valueToLong(v1: V1): Long

//   /** retrieve value from cell **/
//   protected def valueAt(d: D, i: Int, j: Int): V1

//   /** construct a data structure with cells specified by ijs updated with value v1 **/
//   protected def newData(d: D, ijs: Iterable[(Int,Int)], v1: V1 ): D

//   /** update the size (number of items inserted) **/
//   protected def newSize(d: D, v1: V1): D

// }

// /** sketch implementation backed with an immutable table **/
// abstract class DenseFrequencySketch[A,H1,V1 : Monoid,T]
// extends FrequencySketch[A,H1,(Vector[Vector[V1]], Long) @@ T,V1]
// with Monoid[(Vector[Vector[V1]], Long) @@ T]
// with Equal[(Vector[Vector[V1]], Long) @@ T]{

//   def cardinality(d: (Vector[Vector[V1]], Long) @@ T) = d._2

//   def equal(d1: (Vector[Vector[V1]], Long) @@ T, d2: (Vector[Vector[V1]], Long) @@ T) =
//     (d1._1 == d2._1) && (d1._2 === d2._2)

//   lazy val zero = tag((Vector.fill(numHashes, width)(implicitly[Monoid[V1]].zero), 0L))

//   def append(d1: (Vector[Vector[V1]], Long) @@ T, d2: => (Vector[Vector[V1]], Long) @@ T) = {
//     // append values in cells together
//     val data = Vector.tabulate(numHashes, width)((i,j) =>
//       valueAt(d1,i,j) |+| valueAt(d2,i,j))

//     // add sizes together
//     val size = d1._2 + d2._2

//     tag((data, size))
//   }

//   protected def tag(d: (Vector[Vector[V1]], Long)) = Tag[(Vector[Vector[V1]], Long), T](d)

//   protected def valueAt(d: (Vector[Vector[V1]], Long) @@ T, i: Int, j: Int): V1 =
//     d._1(i)(j)

//   protected def newData(d: (Vector[Vector[V1]], Long) @@ T, ijs: Iterable[(Int,Int)], v1: V1): (Vector[Vector[V1]], Long) @@ T = {
//     val updatedTable = ijs.foldLeft(d._1)((table, ij) => {
//       val u = updateValueWith(valueAt(d, ij._1, ij._2), v1)

//       table.updated(ij._1,
//                     table(ij._1).updated(ij._2, u))
//     })

//     tag((updatedTable, d._2))
//   }

//   protected def newSize(d: (Vector[Vector[V1]], Long) @@ T, v1: V1) =
//     tag(d._1, d._2 + valueToLong(v1))

// }

// /** standard frequency counting sketch **/
// abstract class DenseFrequencySketchLongT[A,H1,T]
// extends DenseFrequencySketch[A,H1,Long,T]{
//   protected def valueToLong(v1: Long): Long = v1
// }

// abstract class DenseFrequencySketchHyLLT[A,H1,T,U](implicit hllinst: SparseHyperLogLogT[A,H1,U])
// extends DenseFrequencySketch[A,H1,SparseHLLRegisters[U],T]{
//   protected def valueToLong(v1: SparseHLLRegisters[U]): Long = hllinst.cardinality(v1)
// }

// trait CountMinSketchParameterEstimates{
//   /** delta is certainty having less than eps **/
//   def optimalNumHashes(delta: Double) = {
//     require((delta gte 0.0) && (delta lte 1.0), "delta must be between 0 and 1")
//     math.ceil(math.log(1.0 / (1 - delta))) toInt
//   }

//   /** eps is max tolerable error **/
//   def optimalWidth(eps: Double) = {
//     require((eps gte 0.0) && (eps lte 1.0), "eps must be between 0 and 1")
//     math.ceil(math.exp(1) / eps) toInt
//   }

//   def optimalParameters(eps: Double, delta: Double) = (optimalNumHashes(delta), optimalWidth(eps))
// }



// object sketch {

//   type SketchTable[V1] = (Vector[Vector[V1]], Long)

//   def apply[A,H1,T](params: (Int,Int), s: Long = 0L,
//                     estimator: (Iterable[Long]) => Long) =
//     new DenseFrequencySketchLongT[A,H1,T]{
//       val (numHashes, width) = params
//       val seed = s

//       protected def estimate(cs: Iterable[Long]): Long = estimator(cs)
//     }

//   object countminsketch
//   extends CountMinSketchParameterEstimates{

//     def apply[A,H1,T](params: (Int,Int), s: Long = 0L) =
//       new DenseFrequencySketchLongT[A,H1,T]{
//         val (numHashes, width) = params
//         val seed = s

//         protected def estimate(cs: Iterable[Long]): Long = cs.min
//       }

//     def withHyLL[A,H1,T,U](params: (Int,Int), s: Long = 0L)(implicit hllinst: SparseHyperLogLogT[A,H1,U]) =
//       new DenseFrequencySketchHyLLT[A,H1,T,U]{
//         val (numHashes, width) = params
//         val seed = s

//         protected def estimate(cs: Iterable[Long]): Long = cs.min
//       }

//   }

// }



