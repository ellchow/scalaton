package scalaton.aggregate.hashed

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.hashing._

/** sketch data structure to store and lookup values given a key, generally used for keeping track of frequencies **/
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

  /** get cells to update **/
  def itemIJs(a: A)(implicit h: H, hconv: HC): Iterable[(Int,Int)] = {
    (0 to numHashes).view zip hashItem(a)
  }

  /** compute estimate given the values extracted from each cell **/
  def estimate(cs: Iterable[Long]): Long

  /** extract long from value stored in cell **/
  def valueToLong(v1: V1): Long

  /** update a cell's value given an input **/
  def updateValueWith(v: V1, u: V1): V1

  /** retrieve value from cell **/
  def valueAt(d: D, i: Int, j: Int): V1

  /** construct a data structure with cells specified by ijs updated with value v1 **/
  def newData(d: D, ijs: Iterable[(Int,Int)], v1: V1 ): D

  /** update the size (number of items inserted) **/
  def newSize(d: D, v1: V1): D

}

/** sketch requiring cell values to be monoids **/
abstract class FrequencySketchMonoidVT[A,H1,D,V1 : Monoid]
extends FrequencySketchT[A,H1,D,V1]{
  def updateValueWith(v: V1, u: V1): V1 = v |+| u
}

/** sketch implementation backed with an immutable table **/
abstract class DenseFrequencySketchMonoidVT[A,H1,V1 : Monoid,T]
extends FrequencySketchMonoidVT[A,H1,(Vector[Vector[V1]], Long) @@ T,V1]
with Monoid[(Vector[Vector[V1]], Long) @@ T]
with Equal[(Vector[Vector[V1]], Long) @@ T]{

  def cardinality(d: (Vector[Vector[V1]], Long) @@ T) = d._2

  def equal(d1: (Vector[Vector[V1]], Long) @@ T, d2: (Vector[Vector[V1]], Long) @@ T) =
    (d1._1 == d2._1) && (d1._2 === d2._2)

  lazy val zero = tag((Vector.fill(numHashes, width)(implicitly[Monoid[V1]].zero), 0L))

  def append(d1: (Vector[Vector[V1]], Long) @@ T, d2: => (Vector[Vector[V1]], Long) @@ T) = {
    // append values in cells together
    val data = Vector.tabulate(numHashes, width)((i,j) =>
      valueAt(d1,i,j) |+| valueAt(d2,i,j))

    // add sizes together
    val size = d1._2 + d2._2

    tag((data, size))
  }

  def tag(d: (Vector[Vector[V1]], Long)) = Tag[(Vector[Vector[V1]], Long), T](d)

  def valueAt(d: (Vector[Vector[V1]], Long) @@ T, i: Int, j: Int): V1 = {
    d._1(i)(j)
  }

  def newData(d: (Vector[Vector[V1]], Long) @@ T, ijs: Iterable[(Int,Int)], v1: V1): (Vector[Vector[V1]], Long) @@ T = {
    val updatedTable = ijs.foldLeft(d._1)((table, ij) => {
      val u = updateValueWith(valueAt(d, ij._1, ij._2), v1)

      table.updated(ij._1,
                    table(ij._1).updated(ij._2, u))
    })

    tag((updatedTable, d._2))
  }

  def newSize(d: (Vector[Vector[V1]], Long) @@ T, v1: V1) =
    tag(d._1, d._2 + valueToLong(v1))

}

/** standard frequency counting sketch **/
abstract class DenseFrequencySketchLongT[A,H1,T]
extends DenseFrequencySketchMonoidVT[A,H1,Long,T]{
  def valueToLong(v1: Long): Long = v1
}


trait CountMinSketchParameterEstimates{
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

object sketch {

  type SketchTable[V1] = (Vector[Vector[V1]], Long)

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



