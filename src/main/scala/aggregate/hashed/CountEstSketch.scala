package scalaton.aggregate.hashed

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._

abstract class CountEstSketchConfig[A,H1,V1] extends HashModdedCollectionConfig[A,H1]{
  def estimate(cs: Iterable[Long]): Long

  def valueToLong(v1: V1): Long

  def updateValueWith(v: V1, u: V1): V1

}

abstract class CountEstSketchMonoidVConfig[A,H1,V1 : Monoid] extends CountEstSketchConfig[A,H1,V1]{
  def updateValueWith(v: V1, u: V1): V1 = v |+| u
}
abstract class DenseCountEstSketchMonoidVConfig[A,H1,V1 : Monoid] extends CountEstSketchMonoidVConfig[A,H1,V1]
abstract class DenseCountEstSketchLongConfig[A,H1] extends DenseCountEstSketchMonoidVConfig[A,H1,Long]{

  def valueToLong(v1: Long): Long = v1

}



abstract class CountEstSketch[A,H1,D,V1,C <: CountEstSketchConfig[A,H1,V1]](val conf: C)
extends HashModdedCollection[A,H1,C]
with UpdatesElementValue[A,H1,Int,D,C,V1]
with LooksUpElementValue[A,H1,Int,D,C,Long]{

  def update(d: D, a: A, v1: V1)(implicit h: H, hconv: HC): D = {
    val ijs = itemIJs(a) toSet

    newSize(newData(d, (i: Int, j: Int) =>
                    ijs.contains((i,j)) ? conf.updateValueWith(valueAt(d,i,j),v1) | valueAt(d,i,j) ),
            v1)

  }

  def lookup(d: D, a: A)(implicit h: H, hconv: HC): Long = {
    val ijs = itemIJs(a)

    conf estimate(ijs map { case(i,j) => conf valueToLong(valueAt(d,i,j)) })
  }

  def itemIJs(a: A)(implicit h: H, hconv: HC): Iterable[(Int,Int)] = {
    (0 to conf.numHashes).view zip conf.hashItem(a)
  }


  def valueAt(d: D, i: Int, j: Int): V1

  def newData(d: D, f: (Int,Int) => V1): D

  def newSize(d: D, v1: V1): D

}

abstract class CountEstSketchMonoidV[A,H1,D,V1 : Monoid,C <: CountEstSketchMonoidVConfig[A,H1,V1]](override val conf: C)
extends CountEstSketch[A,H1,D,V1,C](conf)


abstract class DenseCountEstSketchMonoidV[A,H1,V1 : Monoid,C <: DenseCountEstSketchMonoidVConfig[A,H1,V1],T](override val conf: C)
extends CountEstSketchMonoidV[A,H1,(Vector[Vector[V1]], Long) @@ T,V1,C](conf)
with Monoid[(Vector[Vector[V1]], Long) @@ T]
with Equal[(Vector[Vector[V1]], Long) @@ T]{

  def equal(d1: (Vector[Vector[V1]], Long) @@ T, d2: (Vector[Vector[V1]], Long) @@ T) =
    (d1._1 == d2._1) && (d1._2 === d2._2)

  val zero = tag((Vector.fill(conf.numHashes, conf.width)(implicitly[Monoid[V1]].zero), 0L))

  def append(d1: (Vector[Vector[V1]], Long) @@ T, d2: => (Vector[Vector[V1]], Long) @@ T) = {
    val data = Vector.tabulate(conf.numHashes, conf.width)((i,j) =>
      valueAt(d1,i,j) |+| valueAt(d2,i,j))

    val size = d1._2 + d2._2

    tag((data, size))
  }

  def tag(d: (Vector[Vector[V1]], Long)) = Tag[(Vector[Vector[V1]], Long), T](d)

  def valueAt(d: (Vector[Vector[V1]], Long) @@ T, i: Int, j: Int): V1 = d._1(i)(j)

  def newData(d: (Vector[Vector[V1]], Long) @@ T, f: (Int,Int) => V1): (Vector[Vector[V1]], Long) @@ T =
    tag((Vector.tabulate(conf.numHashes, conf.width)(f), d._2))

  def newSize(d: (Vector[Vector[V1]], Long) @@ T, v1: V1) =
    tag(d._1, d._2 + conf.valueToLong(v1))

}

abstract class DenseCountEstSketchLong[A,H1,T](override val conf: DenseCountEstSketchLongConfig[A,H1])
extends DenseCountEstSketchMonoidV[A,H1,Long,DenseCountEstSketchLongConfig[A,H1],T](conf)


object sketch{

  object ces extends UpdatesElementValueFunction with LooksUpElementValueFunction{

    def denseLong[A,H1,T](params: (Int,Int), s: Long = 0L,
                          estimator: (Iterable[Long]) => Long = (x:Iterable[Long]) => x.min) = {
      val conf = new DenseCountEstSketchLongConfig[A,H1] {
        val (numHashes, width) = params
        val seed = s

        def estimate(cs: Iterable[Long]): Long = estimator(cs)
      }
      new DenseCountEstSketchLong[A,H1,T](conf){}
    }
  }
}

