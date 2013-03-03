package scalaton.aggregate.hashed

import scala.collection.mutable

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.hashing._


trait FrequencySketchT[A,H1,D,V1]
extends DoubleHashModdedCollection[A,H1]
with UpdatesElementValue[A,H1,Int,D,V1]
with LooksUpElementValue[A,H1,Int,D,Long]
with Sized[A,H1,Int,D]{

  def update(d: D, a: A, v1: V1)(implicit h: H, hconv: HC): D = {
    val ijs = itemIJs(a)
    newSize(newData(d, ijs, v1), v1)
  }

  def lookup(d: D, a: A)(implicit h: H, hconv: HC): Long = {
    val ijs = itemIJs(a)

    estimate(ijs map { case(i,j) => valueToLong(valueAt(d,i,j)) })
  }

  def itemIJs(a: A)(implicit h: H, hconv: HC): Iterable[(Int,Int)] = {
    (0 to numHashes).view zip hashItem(a)
  }

  def estimate(cs: Iterable[Long]): Long

  def valueToLong(v1: V1): Long

  def updateValueWith(v: V1, u: V1): V1

  def valueAt(d: D, i: Int, j: Int): V1

  def newData(d: D, ijs: Iterable[(Int,Int)], v1: V1 ): D

  def newSize(d: D, v1: V1): D

}

abstract class FrequencySketchMonoidVT[A,H1,D,V1 : Monoid]
extends FrequencySketchT[A,H1,D,V1]{
  def updateValueWith(v: V1, u: V1): V1 = v |+| u
}


abstract class DenseFrequencySketchMonoidVT[A,H1,V1 : Monoid,T]
extends FrequencySketchMonoidVT[A,H1,(mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long) @@ T,V1]
with Monoid[(mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long) @@ T]
with Equal[(mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long) @@ T]{

  def cardinality(d: (mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long) @@ T) = d._2

  def equal(d1: (mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long) @@ T, d2: (mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long) @@ T) =
    (d1._1 == d2._1) && (d1._2 === d2._2)

  def zero = tag((mutable.ArrayBuffer.fill(numHashes, width)(implicitly[Monoid[V1]].zero), 0L))

  def dim(d: mutable.ArrayBuffer[mutable.ArrayBuffer[V1]]): (Int, Int) = (d.length, d(0).length)

  def append(d1: (mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long) @@ T, d2: => (mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long) @@ T) = {
    val data = mutable.ArrayBuffer.tabulate(numHashes, width)((i,j) =>
      valueAt(d1,i,j) |+| valueAt(d2,i,j))

    val size = d1._2 + d2._2

    tag((data, size))
  }

  def tag(d: (mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long)) = Tag[(mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long), T](d)

  def valueAt(d: (mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long) @@ T, i: Int, j: Int): V1 = {
    d._1(i)(j)
  }

  def newData(d: (mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long) @@ T, ijs: Iterable[(Int,Int)], v1: V1): (mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long) @@ T = {
    ijs foreach { case (i, j) => d._1(i)(j) = updateValueWith(valueAt(d,i,j),v1) }

    d
  }


  def newSize(d: (mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long) @@ T, v1: V1) =
    tag(d._1, d._2 + valueToLong(v1))


}

abstract class DenseFrequencySketchLongT[A,H1,T]
extends DenseFrequencySketchMonoidVT[A,H1,Long,T]{

  def valueToLong(v1: Long): Long = v1

}


object sketch extends UpdatesElementValueFunction
              with LooksUpElementValueFunction
              with MakesSingletonFunction{

  type SketchTable[V1] = (mutable.ArrayBuffer[mutable.ArrayBuffer[V1]], Long)

  def apply[A,H1,T](params: (Int,Int), s: Long = 0L,
                    estimator: (Iterable[Long]) => Long) =
    new DenseFrequencySketchLongT[A,H1,T]{
      val (numHashes, width) = params
      val seed = s

      def estimate(cs: Iterable[Long]): Long = estimator(cs)
    }

  object countminsketch {

    def apply[A,H1,T](params: (Int,Int), s: Long = 0L) =
      new DenseFrequencySketchLongT[A,H1,T]{
        val (numHashes, width) = params
        val seed = s

        def estimate(cs: Iterable[Long]): Long = cs.min
      }

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



}

