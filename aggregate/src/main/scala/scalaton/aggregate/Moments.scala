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

import org.apache.commons.math3.distribution.PoissonDistribution

trait MomentsModule{

  // http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Higher-order_statistics
  implicit val momentsMonoid: Monoid[Moments] = Monoid instance ( (xa, xb) => {
    val stabilityRatio = 0.1

    val delta = xb.mean - xa.mean

    val n = xa.n + xb.n

    val mean = {
      val (xsmall, xlarge) = ((xa.n lt xb.n) ? (xa, xb) | (xb, xa))

      val ratio = xsmall.n.toDouble / n

      if(ratio lt stabilityRatio)
        xlarge.mean + (xsmall.mean - xlarge.mean) * ratio
      else
        (xa.n * xa.mean + xb.n * xb.mean) / n
    }

    val m2 = xa.m2 + xb.m2 +
    ((delta * delta) * xa.n * xb.n / n)

    val m3 = xa.m3 + xb.m3 +
    ((delta * delta * delta) * xa.n * xb.n * (xa.n - xb.n) / (n * n)) +
    (3 * delta * (xa.n * xb.m2 - xb.n * xa.m2) / n)

    val m4 = xa.m4 + xb.m4 +
    ((delta * delta * delta * delta) * (xa.n * xb.n * ((xa.n * xa.n - xa.n * xb.n + xb.n * xb.n))) / (n * n * n)) +
    (6 * (delta * delta) * (xa.n * xa.n * xb.m2 + xb.n * xb.n * xa.m2) / (n * n)) +
    (4 * delta * (xa.n * xb.m3 - xb.n * xa.m3) / n)

    Moments(n, mean, m2, m3, m4)
  },
    Moments(0,0,0,0,0)
  )

  object Moments{
    def apply(x: Double): Moments = Moments(1L, x, 0.0, 0.0, 0.0)

    def from(xs: Iterable[Double]): Moments = xs.foldLeft(momentsMonoid.zero)((m, x) => m consume x)

  }
  case class Moments(val n: Long, val mean: Double, val m2: Double, val m3: Double, val m4: Double){
    def variance = m2 / n

    def stdev = math.sqrt(variance)

    def skewness = (math.sqrt(n)) * m3 / (math.pow(m2, 1.5))

    def kurtosis = (n * m4) / (m2 * m2) - 3

    def consume(x: Double) = Moments(x) |+| this
  }

  trait BootstrappedMoments[T] extends Monoid[Map[Int,Moments] @@ T]{
    val b: Int
    val poisson: PoissonDistribution

    def tag(m: Map[Int,Moments]) = Tag[Map[Int,Moments], T](m)

    val zero = tag(Map[Int,Moments]())

    def append(xa: Map[Int,Moments] @@ T, xb: => Map[Int,Moments] @@ T) =
      tag((xa: Map[Int,Moments]) |+| (xb: Map[Int,Moments]))

  }

  object BootstrappedMoments{
    def apply[T](x: Double)(implicit bmMonoid: BootstrappedMoments[T]): Map[Int,Moments] @@ T = {
      val xs = for{
        i <- (0 until bmMonoid.b).view
        _ <- (0 until bmMonoid.poisson.sample).view
      } yield Map(i -> Moments(x))

      bmMonoid.tag(xs.reduce(_ |+| _))
    }
  }
}

object moments extends MomentsModule
