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

abstract class Sketch[K,V : Monoid,W,D,T <: Sketch[K,V,W,D,T]] extends DoubleHashModdedCollection[K] {
  val data: D
  val size: Long

  protected def apply(d: D, n: Long): T
  protected def readCell(d: D, i: Int, j: Int): V
  protected def updateCell(d: D, i: Int, j: Int, v: V): D
  protected def estimate(vs: Iterable[V]): W

  def getCells(k: K): Iterable[(Int, Int)] = (0 until numHashes).view.zip(hashItem(k))

  def +(kv: (K,V), vsize: Long = 1L): T = {
    val (k, v) = kv
    val cells = getCells(k)
    val d = cells.foldLeft(data){ case (dd, (i, j)) => updateCell(dd, i, j, v) }

    apply(d, size + vsize)
  }

  def apply(k: K): W = {
    val cells = getCells(k)
    estimate(cells.map{ case (i, j) => readCell(data, i, j) })
  }

  def merge(x: T): T = {
    require(isCompatible(x), "incompatible sketches")

    val cells = for{
      i <- (0 until numHashes).view
      j <- (0 until width)
    } yield (i, j)

    val d = cells.foldLeft(data){ case (dd, (i, j)) => updateCell(dd, i, j, readCell(x.data, i, j)) }

    apply(d, size + x.size)
  }
}

abstract class  VectorBackedSketch[K,V : Monoid,W,T <: VectorBackedSketch[K,V,W,T]](data: Vector[Vector[V]], size: Long, numHashes: Int, width: Int, seed: Long = 0)(implicit val hashable: Hashable32[K]) extends Sketch[K,V,W,Vector[Vector[V]],T] {
  def readCell(d: Vector[Vector[V]], i: Int, j: Int) = d(i)(j)

  def updateCell(d: Vector[Vector[V]], i: Int, j: Int, v: V) =
    d.updated(i, d(i).updated(j, readCell(d, i, j) |+| v))
}

case class CountMinSketch[K : Hashable32](data: Vector[Vector[Long]], size: Long, numHashes: Int, width: Int, seed: Long = 0) extends VectorBackedSketch[K,Long,Long,CountMinSketch[K]](data,size,numHashes,width,seed) {
  def apply(d: Vector[Vector[Long]], n: Long) = this.copy(data = d, size = size)
  def estimate(vs: Iterable[Long]) = vs.min
}
trait CountMinSketchParameters {
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
