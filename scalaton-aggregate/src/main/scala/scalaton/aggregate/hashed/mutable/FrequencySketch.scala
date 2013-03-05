package scalaton.aggregate.hashed.mutable

import scalaz._
import Scalaz._

import scalaton.aggregate.hashed._

import scalaton.util._
import scalaton.util.hashing._

/** sketch implementation backed with an mutable table **/
abstract class DenseFrequencySketchMonoidVT[A,H1,V1 : Monoid,T]
extends FrequencySketchMonoidVT[A,H1,(collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long) @@ T,V1]
with Monoid[(collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long) @@ T]
with Equal[(collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long) @@ T]{

  def cardinality(d: (collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long) @@ T) = d._2

  def equal(d1: (collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long) @@ T, d2: (collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long) @@ T) =
    (d1._1 == d2._1) && (d1._2 === d2._2)

  def zero = tag((collection.mutable.ArrayBuffer.fill(numHashes, width)(implicitly[Monoid[V1]].zero), 0L))

  def dim(d: collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]]): (Int, Int) = (d.length, d(0).length)

  def append(d1: (collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long) @@ T, d2: => (collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long) @@ T) = {
    val data = collection.mutable.ArrayBuffer.tabulate(numHashes, width)((i,j) =>
      valueAt(d1,i,j) |+| valueAt(d2,i,j))

    val size = d1._2 + d2._2

    tag((data, size))
  }

  def tag(d: (collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long)) = Tag[(collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long), T](d)

  def valueAt(d: (collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long) @@ T, i: Int, j: Int): V1 = {
    d._1(i)(j)
  }

  def newData(d: (collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long) @@ T, ijs: Iterable[(Int,Int)], v1: V1): (collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long) @@ T = {
    ijs foreach { case (i, j) => d._1(i)(j) = updateValueWith(valueAt(d,i,j),v1) }

    d
  }

  def newSize(d: (collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long) @@ T, v1: V1) =
    tag(d._1, d._2 + valueToLong(v1))


}

/** standard frequency counting sketch using mutable table **/
abstract class DenseFrequencySketchLongT[A,H1,T]
extends DenseFrequencySketchMonoidVT[A,H1,Long,T]{
  def valueToLong(v1: Long): Long = v1
}


object sketch {

  type SketchTable[V1] = (collection.mutable.ArrayBuffer[collection.mutable.ArrayBuffer[V1]], Long)

  def apply[A,H1,T](params: (Int,Int), s: Long = 0L,
                    estimator: (Iterable[Long]) => Long) =
    new DenseFrequencySketchLongT[A,H1,T]{
      val (numHashes, width) = params
      val seed = s

      def estimate(cs: Iterable[Long]): Long = estimator(cs)
    }

  object countminsketch
  extends CountMinSketchParameterEstimates{

    def apply[A,H1,T](params: (Int,Int), s: Long = 0L) =
      new DenseFrequencySketchLongT[A,H1,T]{
        val (numHashes, width) = params
        val seed = s

        def estimate(cs: Iterable[Long]): Long = cs.min
      }
  }
}

