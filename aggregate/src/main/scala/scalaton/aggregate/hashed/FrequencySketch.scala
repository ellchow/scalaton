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

  implicit def sketchSemigroup[F[_], V, W](implicit monoidV: Monoid[V], skt: SketchTable[F,V], sk: Sketch[F, V, W]): Semigroup[F[V]] = new Semigroup[F[V]]{
    def append(d1: F[V], d2: => F[V]) = sk.merge(d1, d2)
  }

  implicit class CountMinSketchOps[F[_], Long](override val d: F[Long])(implicit monoidV: Monoid[Long], skt: SketchTable[F,Long], sk: Sketch[F, Long, Long]) extends SketchOps[F, Long, Long](d)

}

object sketch extends SketchModule
