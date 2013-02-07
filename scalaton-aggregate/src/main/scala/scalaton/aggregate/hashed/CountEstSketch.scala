package scalaton.aggregate.hashed

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.hashing._


trait CountEstSketchT[A,H1,D,V1]
extends HashModdedCollection[A,H1]
with UpdatesElementValue[A,H1,Int,D,V1]
with LooksUpElementValue[A,H1,Int,D,Long]{

  def update(d: D, a: A, v1: V1)(implicit h: H, hconv: HC): D = {
    val ijs = itemIJs(a) toSet

    newSize(newData(d, (i: Int, j: Int) =>
                    ijs.contains((i,j)) ? updateValueWith(valueAt(d,i,j),v1) | valueAt(d,i,j) ),
            v1)

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

  def newData(d: D, f: (Int,Int) => V1): D

  def newSize(d: D, v1: V1): D

}

abstract class CountEstSketchMonoidVT[A,H1,D,V1 : Monoid]
extends CountEstSketchT[A,H1,D,V1]{
  def updateValueWith(v: V1, u: V1): V1 = v |+| u
}


abstract class DenseCountEstSketchMonoidVT[A,H1,V1 : Monoid,T]
extends CountEstSketchMonoidVT[A,H1,(Vector[Vector[V1]], Long) @@ T,V1]
with Monoid[(Vector[Vector[V1]], Long) @@ T]
with Equal[(Vector[Vector[V1]], Long) @@ T]{

  def equal(d1: (Vector[Vector[V1]], Long) @@ T, d2: (Vector[Vector[V1]], Long) @@ T) =
    (d1._1 == d2._1) && (d1._2 === d2._2)

  lazy val zero = tag((Vector.fill(numHashes, width)(implicitly[Monoid[V1]].zero), 0L))

  def append(d1: (Vector[Vector[V1]], Long) @@ T, d2: => (Vector[Vector[V1]], Long) @@ T) = {
    val data = Vector.tabulate(numHashes, width)((i,j) =>
      valueAt(d1,i,j) |+| valueAt(d2,i,j))

    val size = d1._2 + d2._2

    tag((data, size))
  }

  def tag(d: (Vector[Vector[V1]], Long)) = Tag[(Vector[Vector[V1]], Long), T](d)

  def valueAt(d: (Vector[Vector[V1]], Long) @@ T, i: Int, j: Int): V1 = {
    d._1(i)(j)
  }

  def newData(d: (Vector[Vector[V1]], Long) @@ T, f: (Int,Int) => V1): (Vector[Vector[V1]], Long) @@ T =
    tag((Vector.tabulate(numHashes, width)(f), d._2))

  def newSize(d: (Vector[Vector[V1]], Long) @@ T, v1: V1) =
    tag(d._1, d._2 + valueToLong(v1))

}

abstract class DenseCountEstSketchLongT[A,H1,T]
extends DenseCountEstSketchMonoidVT[A,H1,Long,T]{

  def valueToLong(v1: Long): Long = v1

}


object sketch extends UpdatesElementValueFunction
              with LooksUpElementValueFunction
              with MakesSingletonFunction{

  object ces {

    def denseLong[A,H1,T](params: (Int,Int), s: Long = 0L,
                          estimator: (Iterable[Long]) => Long) =
      new DenseCountEstSketchLongT[A,H1,T]{
        val (numHashes, width) = params
        val seed = s

        def estimate(cs: Iterable[Long]): Long = estimator(cs)
      }

  }

  object countminsketch {

    def apply[A,H1,T](params: (Int,Int), s: Long = 0L) =
      new DenseCountEstSketchLongT[A,H1,T]{
        val (numHashes, width) = params
        val seed = s

        def estimate(cs: Iterable[Long]): Long = cs.min
      }

    /** delta is certainty having less than eps **/
    def optimalNumHashes(delta: Double) = {
      require((delta gte 0.0) && (delta lte 1.0), "delta must be between 0 and 1")
      math.ceil(math.log(1 - delta) / math.log(0.5)) toInt
    }

    /** eps is max tolerable error **/
    def optimalWidth(eps: Double) = {
      require((eps gte 0.0) && (eps lte 1.0), "eps must be between 0 and 1")
      math.ceil(2 / eps) toInt
    }

    def optimalParameters(eps: Double, delta: Double) = (optimalNumHashes(delta), optimalWidth(eps))
  }

}

