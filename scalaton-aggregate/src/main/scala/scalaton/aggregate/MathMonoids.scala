package scalaton.aggregate

import scalaz._
import Scalaz._

sealed trait DenseVec
sealed trait SparseVec

object math
extends VecInstances
with VecFunctions
with MathMonoids{

}

trait VecFunctions {
  type DVec[A] = Seq[A] @@ DenseVec
  type SVec[K,V] = Map[K,V] @@ SparseVec

  def Vec[A : Semigroup](x: A*): DVec[A] = Tag(x toVector)

  def Vec[K,V : Monoid](x: (K,V)*): SVec[K,V] = Tag(x toMap)

  def tag[A, T](u: DVec[A])(implicit at : Semigroup[A @@ T]) =
    Vec(Tag.subst[A,Seq,T](u : Seq[A]) : _*)

  def tag[K, V, T](u: SVec[K,V])(implicit at : Monoid[V @@ T]): SVec[K, V @@ T] =
    Vec(u.view.map{case (k,v) => (k, Tag[V,T](v)) }.toSeq : _*)

  def multiply[A](u1: DVec[A], u2: DVec[A])(implicit at : Monoid[A @@ Tags.Multiplication],
                                            vs: Monoid[DVec[A @@ Tags.Multiplication]]) =
    tag[A,Tags.Multiplication](u1) |+| tag[A,Tags.Multiplication](u2)

  // def multiply[K,V](u1: SVec[K,V], u2: SVec[K,V])
  //                  (implicit vt : Monoid[V @@ Tags.Multiplication],
  //                   vs: Monoid[SVec[K,V @@ Tags.Multiplication]]) =
  //   tag[K,V,Tags.Multiplication](u1) |+| tag[K,V,Tags.Multiplication](u2)



  // def innerProduct[A](u: DVec[A], v: DVec[A])
  //                    (implicit addition: Monoid[A],
  //                     multiplication: Monoid[A @@ Tags.Multiplication],
  //                     vs: Semigroup[DVec[A]]) =


}

trait VecInstances { this: VecFunctions =>
  implicit def denseVecSemigroup[A : Semigroup]: Semigroup[DVec[A]] =
    Semigroup instance ((x, y) => {
      require(x.size == y.size, "vec x and y must have same length")
      Vec(x.zip(y).map{ case (xi, yi) => xi |+| yi } : _*)
    })

  implicit def sparseVecMonoid[K,V : Monoid]: Monoid[SVec[K,V]] =
    Monoid instance ((x,y) => Vec(((x : Map[K,V]) |+| (y : Map[K,V])).toSeq : _*),
                     Vec[K,V]())


}

trait MathMonoids{

  implicit val byteMinMonoidInstance: Monoid[Byte @@ Tags.Min] =
    Monoid instance ((x,y) => Tag(x min y), Tag(Byte.MaxValue))

  implicit val byteMaxMonoidInstance: Monoid[Byte @@ Tags.Max] =
    Monoid instance ((x,y) => Tag(x max y), Tag(Byte.MinValue))

  implicit val shortMinMonoidInstance: Monoid[Short @@ Tags.Min] =
    Monoid instance ((x,y) => Tag(x min y), Tag(Short.MaxValue))

  implicit val shortMaxMonoidInstance: Monoid[Short @@ Tags.Max] =
    Monoid instance ((x,y) => Tag(x max y), Tag(Short.MinValue))

  implicit val charMinMonoidInstance: Monoid[Char @@ Tags.Min] =
    Monoid instance ((x,y) => Tag(x min y), Tag(Char.MaxValue))

  implicit val charMaxMonoidInstance: Monoid[Char @@ Tags.Max] =
    Monoid instance ((x,y) => Tag(x max y), Tag(Char.MinValue))

  implicit val intMinMonoidInstance: Monoid[Int @@ Tags.Min] =
    Monoid instance ((x,y) => Tag(x min y), Tag(Int.MaxValue))

  implicit val intMaxMonoidInstance: Monoid[Int @@ Tags.Max] =
    Monoid instance ((x,y) => Tag(x max y), Tag(Int.MinValue))

  implicit val longMinMonoidInstance: Monoid[Long @@ Tags.Min] =
    Monoid instance ((x,y) => Tag(x min y), Tag(Long.MaxValue))

  implicit val longMaxMonoidInstance: Monoid[Long @@ Tags.Max] =
    Monoid instance ((x,y) => Tag(x max y), Tag(Long.MinValue))

  implicit val floatMinMonoidInstance: Monoid[Float @@ Tags.Min] =
    Monoid instance ((x,y) => Tag(x min y), Tag(Float.MaxValue))

  implicit val floatMaxMonoidInstance: Monoid[Float @@ Tags.Max] =
    Monoid instance ((x,y) => Tag(x max y), Tag(Float.MinValue))

  implicit val doubleMinMonoidInstance: Monoid[Double @@ Tags.Min] =
    Monoid instance ((x,y) => Tag(x min y), Tag(Double.MaxValue))

  implicit val doubleMaxMonoidInstance: Monoid[Double @@ Tags.Max] =
    Monoid instance ((x,y) => Tag(x max y), Tag(Double.MinValue))

}
