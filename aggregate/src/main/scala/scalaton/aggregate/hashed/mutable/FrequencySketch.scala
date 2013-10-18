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

package scalaton.aggregate.hashed.mutable

import collection.mutable

import scalaz._
import Scalaz._

import scalaton.aggregate.hashed._

import scalaton.util._
import scalaton.util.hashing._


case class MutableSketchTable[V1](val table: mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], var size: Long)

/** sketch implementation backed with an mutable table **/
abstract class DenseFrequencySketchMonoidVT[A,H1,V1 : Monoid,T]
extends FrequencySketchMonoidVT[A,H1,MutableSketchTable[V1] @@ T,V1]
with Monoid[MutableSketchTable[V1] @@ T]
with Equal[MutableSketchTable[V1] @@ T]{

  def cardinality(d: MutableSketchTable[V1] @@ T) = d.size

  def equal(d1: MutableSketchTable[V1] @@ T,
            d2: MutableSketchTable[V1] @@ T) =
    (d1.table == d2.table) && (d1.size === d2.size)

  def zero = tag(MutableSketchTable(mutable.ArrayBuffer.fill(numHashes, width)(implicitly[Monoid[V1]].zero), 0L))

  def append(d1: MutableSketchTable[V1] @@ T,
             d2: => MutableSketchTable[V1] @@ T) = {
    val data = mutable.ArrayBuffer.tabulate(numHashes, width)((i,j) =>
      valueAt(d1,i,j) |+| valueAt(d2,i,j))

    val size = d1.size + d2.size

    tag(MutableSketchTable(data, size))
  }

  protected def tag(d: MutableSketchTable[V1]) = Tag[MutableSketchTable[V1], T](d)

  protected def dim(d: mutable.ArrayBuffer[mutable.ArrayBuffer[V1]]): (Int, Int) = (d.length, d(0).length)

  protected def valueAt(d: MutableSketchTable[V1] @@ T, i: Int, j: Int): V1 = {
    d.table(i)(j)
  }

  protected def newData(d: MutableSketchTable[V1] @@ T, ijs: Iterable[(Int,Int)], v1: V1): MutableSketchTable[V1] @@ T = {
    ijs foreach { case (i, j) => d.table(i)(j) = updateValueWith(valueAt(d,i,j),v1) }

    d
  }

  protected def newSize(d: MutableSketchTable[V1] @@ T, v1: V1) = {
    d.size = d.size + valueToLong(v1)

    d
  }

}

/** standard frequency counting sketch using mutable table **/
abstract class DenseFrequencySketchLongT[A,H1,T]
extends DenseFrequencySketchMonoidVT[A,H1,Long,T]{
  protected def valueToLong(v1: Long): Long = v1
}


object sketch {

  type SketchTable[V1] = MutableSketchTable[V1]

  def apply[A,H1,T](params: (Int,Int), s: Long = 0L,
                    estimator: (Iterable[Long]) => Long) =
    new DenseFrequencySketchLongT[A,H1,T]{
      val (numHashes, width) = params
      val seed = s

      protected def estimate(cs: Iterable[Long]): Long = estimator(cs)
    }

  object countminsketch
  extends CountMinSketchParameterEstimates{

    def apply[A,H1,T](params: (Int,Int), s: Long = 0L) =
      new DenseFrequencySketchLongT[A,H1,T]{
        val (numHashes, width) = params
        val seed = s

        protected def estimate(cs: Iterable[Long]): Long = cs.min
      }
  }
}

