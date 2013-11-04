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

package scalaton.aggregate

import scala.collection.mutable

import scalaton.util.tag._

import scalaz._
import Scalaz._

import org.la4j.{vector => lav}

trait la4sModule{
  trait Logical

  implicit def vectorToVec(v: lav.Vector): Vec = Vec(v)

  object Vec{
    def dense(xs: Double*): Vec =
      Vec(new lav.dense.BasicVector(xs.toArray))

    def sparse(len: Int, xs: (Int, Double)*): Vec =
      Vec(new lav.sparse.CompressedVector(len, xs.size, xs.map(_._2).toArray, xs.map(_._1).toArray))

    def elementwiseNonzero(u: Vec, v: Vec)(f: (Double, Double) => Double) = {
      val allIndices = u.nonzeroIndices ++ v.nonzeroIndices

      val res = u.vector.blank

      allIndices.foreach(i => res.set(i, f(u(i), v(i))))

      Vec(res)
    }

    def elementwise(u: Vec, v: Vec)(f: (Double, Double) => Double) = {
      val res = u.vector.blank

      val n = u.size

      @annotation.tailrec
      def loop(i: Int): Unit = {
        if(i >= n){
          Unit
        }else{
          res.set(i, f(u(i), v(i)))

          loop(i + 1)
        }
      }

      loop(0)

      Vec(res)
    }

    def ifelse(mask: Vec @@ Logical, ifTrue: Vec, ifFalse: Vec) =
      elementwiseNonzero(mask, ifTrue)(_ * _) + elementwiseNonzero(1 -: mask, ifFalse)(_ * _)
  }

  case class Vec(private[aggregate] val vector: lav.Vector){
    def apply(i: Int) = vector.get(i)

    def get(i: Int) = if((i > 0) && (i < size)) apply(i).some else none

    def cardinality = vector match {
      case sv: lav.sparse.SparseVector => sv.cardinality
      case _ => size
    }

    def size = vector.length

    def + (u: Vec): Vec = Vec(vector add u.vector)

    def +: (k: Double): Vec = Vec(vector add k)

    def :+ (k: Double): Vec = Vec(vector add k)

    def - (u: Vec): Vec = Vec(vector subtract u.vector)

    def :- (k: Double): Vec = Vec(vector subtract k)

    def -: (k: Double): Vec = Vec((vector multiply -1.0) add 1)

    def :* (k: Double): Vec = Vec(vector multiply k)

    def *: (k: Double): Vec = Vec(vector multiply k)

    def :/ (k: Double): Vec = Vec(vector divide k)

    def innerProduct (u: Vec): Double = vector innerProduct u.vector

    def dot(u: Vec) = innerProduct(u)

    def foreachi(nonzero: Boolean)(f: (Int, => Double) => Unit) = vector match {
      case sv: lav.sparse.SparseVector if nonzero =>
        val g = new lav.functor.VectorProcedure{
          def apply(i: Int, value: Double): Unit = f(i, value)
        }

        vector.eachNonZero(g)

      case _ =>
        val n = size

        @annotation.tailrec
        def loop(i: Int): Unit = {
          lazy val value = apply(i)

          if(i >= n){
            Unit
          }else{
            if(!nonzero || (value > lav.Vectors.EPS))
              f(i, value)

            loop(i + 1)
          }
        }

        loop(0)
    }

    def foreach(f: Double => Unit) =
      foreachi(false)((i, value) => f(value))

    def foreachNonzero(f: Double => Unit) =
      foreachi(true)((i, value) => f(value))

    def mapi(nonzero: Boolean)(f: (Int, => Double) => Double) = {
      val u = vector.blank

      foreachi(nonzero)( (i, value) => u.set(i, f(i, value)) )

      Vec(u)
    }

    def map(f: Double => Double) =
      mapi(false)((i, value) => f(value))

    def mapNonzero(f: Double => Double) =
      mapi(true)((i, value) => f(value))

    def foldi[B](init: B, nonzero: Boolean = false)(f: (B, Int, Double) => B ): B = {
      var b = init

      foreachi(nonzero){ (i, value) => b = f(b, i, value) }

      b
    }

    def fold[B](init: B)(f: (B, Double) => B ): B =
      foldi(init, false)((b, i, value) => f(b, value))

    def foldNonZero[B](init: B)(f: (B, Double) => B ): B =
      foldi(init, true)((b, i, value) => f(b, value))

    def foldMap[B : Monoid](f: Double => B) =
      fold(implicitly[Monoid[B]].zero)((b, value) => b |+| f(value))

    def foldMapNonzero[B : Monoid](f: Double => B) =
      foldNonZero(implicitly[Monoid[B]].zero)((b, value) => b |+| f(value))

    def nonzeroIndices =
      foldi(collection.immutable.BitSet(), true)((buf, i, value) => buf + i)


    def sum = foldNonZero(0.0)(_ + _)

    def max = foldNonZero(0.0)(_ max _)

    def min = foldNonZero(0.0)(_ min _)

    def eq(y: Double) = map(x => if(math.abs(x - y) < lav.Vectors.EPS) 1.0 else 0.0).tag[Logical]

    def lt(y: Double) = map(x => if(x < y) 1.0 else 0.0).tag[Logical]

    def gt(y: Double) = map(x => if(x > y) 1.0 else 0.0).tag[Logical]

    def gte(y: Double) = map(x => if((x - y) > -lav.Vectors.EPS) 1.0 else 0.0).tag[Logical]

    def lte(y: Double) = map(x => if((x - y) < lav.Vectors.EPS) 1.0 else 0.0).tag[Logical]

    // elementwise ops

    def max_(v: Vec) = Vec.elementwiseNonzero(vector,v)(_ max _)

    def min_(v: Vec) = Vec.elementwiseNonzero(vector,v)(_ min _)

    def eq_(v: Vec) = Vec.elementwiseNonzero(vector,v)((uu, vv) => if(math.abs(uu - vv) < lav.Vectors.EPS) 1.0 else 0).tag[Logical]

    def gt_(v: Vec) = Vec.elementwiseNonzero(vector,v)((uu, vv) => if(uu > vv) 1.0 else 0).tag[Logical]

    def lt_(v: Vec) = Vec.elementwiseNonzero(vector,v)((uu, vv) => if(uu < vv) 1.0 else 0).tag[Logical]

    def gte_(v: Vec) = Vec.elementwiseNonzero(vector,v)((uu, vv) => if((uu - vv) > -lav.Vectors.EPS) 1.0 else 0).tag[Logical]

    def lte_(v: Vec) = Vec.elementwiseNonzero(vector,v)((uu, vv) => if((uu - vv) < lav.Vectors.EPS) 1.0 else 0).tag[Logical]
  }
}

object la4s extends la4sModule
