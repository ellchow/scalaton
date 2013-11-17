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

import scalaz._
import Scalaz._

object bootstrapping{
  type Poisson = org.apache.commons.math3.distribution.PoissonDistribution

  abstract class Bootstrapped[A : Monoid, T] extends Monoid[Map[Int, A] @@ T]{
    val b: Int
    val poisson: Poisson

    def tag(m: Map[Int,A]) = Tag[Map[Int,A], T](m)

    val zero = tag(Map[Int,A]())

    def append(xa: Map[Int,A] @@ T, xb: => Map[Int,A] @@ T) =
      tag((xa: Map[Int, A]) |+| (xb: Map[Int, A]))

  }


  def bootstrapped[A : Monoid, T](rounds: Int, poi: Poisson) = new Bootstrapped[A, T]{
      val b = rounds
      val poisson = poi
    }

  def init[A,T](x: A)(implicit mon: Monoid[A], bts: Bootstrapped[A,T]): Map[Int, A] @@ T = {
    val xs = for{
      i <- (0 until bts.b).view
      _ <- (0 until bts.poisson.sample).view
    } yield Map(i -> x)

    bts.tag(xs.reduce(_ |+| _))
  }
}
