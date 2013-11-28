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

trait BootstrappingModule{
  type Poisson = org.apache.commons.math3.distribution.PoissonDistribution

  case class Bootstrapped[A : Monoid](val b: Int, val poisson: Poisson, val m: Map[Int,A]){
    def isCompatibleWith(that: Bootstrapped[A]): Boolean =
      (this.b === that.b) && (this.poisson == that.poisson)

    def insert(x: A) = {
      val xs = for{
      i <- (0 until b).view
      _ <- (0 until poisson.sample).view
    } yield Map(i -> x)

      Bootstrapped(b, poisson, xs.foldLeft(m)(_ |+| _))
    }

    def merge(that: Bootstrapped[A]) = {
      require(isCompatibleWith(that))

      Bootstrapped(b, poisson, m |+| that.m)
    }
  }

  object Bootstrapped{
    def fromData[A : Monoid](b: Int, poisson: Poisson)(xs: Iterable[A]) =
      xs.foldLeft(Bootstrapped(b, poisson, Map[Int,A]()))((b, a) => b.insert(a))
  }

  implicit def bootstrappedSemigroup[A]: Semigroup[Bootstrapped[A]] = new Semigroup[Bootstrapped[A]]{
    def append(b1: Bootstrapped[A], b2: => Bootstrapped[A]) = b1 merge b2
  }
}

object  bootstrapping extends BootstrappingModule
