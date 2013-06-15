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

import collection.immutable.{Vector => SVector}

import breeze.linalg._

import scalaz._
import Scalaz._

trait SGDModule{
  object sgdtypes{
    type Features = Vector[Double]
    type Target = Double
    type Example = (Target, Features)
    type Weights = Vector[Double]
    type Gradient = Vector[Double]
    type TotalPenalty = Double
    type ActualPenalty = Vector[Double]
    type NumExamples = Long
    type RegularizationParameter = Double

    type LearningRateFunction = NumExamples => Double
    type PenaltyFunction = (Weights, Features, TotalPenalty, ActualPenalty) => (Weights, ActualPenalty)
    type GradientFunction = (Weights, Example) => Gradient
  }
  import sgdtypes._

  object sgd{
    def weights(w: Seq[Double]): Weights = DenseVector((0.0 +: w) : _*)

    def weights(w: Seq[(Int,Double)], size: Int, unsorted: Boolean): Weights = {
      val (indices, values) = (unsorted ? w.sorted | w)
        .foldLeft((SVector[Int](0), SVector[Double](0.0))){ case ((i, v), (ii, vv)) =>
                                                            (i :+ (ii + 1), v :+ vv)
                                                          }

      new SparseVector(indices.toArray, values.toArray, size + 1)
    }

    def example(y: Double, x: Seq[Double]): Example = (y, DenseVector((1.0 +: x) : _*))

    def example(y: Double, x: Seq[(Int,Double)], size: Int, unsorted: Boolean): Example = (y, weights(x, size, unsorted))

    def update(gradient: GradientFunction)(learningRate: LearningRateFunction, penalize: PenaltyFunction, c: RegularizationParameter)(w0: Weights, u0: TotalPenalty, q0: ActualPenalty, k0: NumExamples)(ex: Example) = {
      val eta = learningRate(k0)
      val k1 = k0 + 1
      val u1 = u0 + c * eta
      val (y, x) = ex

      val (w1, q1) = penalize(w0 - (gradient(w0, (y, x)) * eta),
                              x,
                              u1,
                              q0)

      (w1, u1, q1, k1)
    }

    def fit(f: (Weights, TotalPenalty, ActualPenalty, NumExamples) => Example => (Weights, TotalPenalty, ActualPenalty, NumExamples), init: Weights)(examples: Iterable[Example]) = {
      examples.foldLeft((init, 0.0: TotalPenalty, SparseVector.zeros[Double](init.size) : ActualPenalty, 0L: NumExamples)){
        case ((w, u, q, k), ex) => f(w, u, q, k)(ex)
      }
    }
  }

  object glm{
    val gaussian = sgd.update{ case (w, (y, x)) => x * ((w dot x) - y) } _

    val bernoulli = sgd.update{ case (w, (y, x)) => x * (logistic(w dot x) - y) } _

    val poisson = sgd.update{ case (w, (y, x)) => x * math.exp(w dot x) - y } _

    def logistic(z: Double) = 1.0 / (1.0 + math.exp(- z))
  }

  object learnrate{
    def constant(c: Double): LearningRateFunction = _ => c
  }

  object penalty{
    val zero: PenaltyFunction = (w, x, u, q) => (w, q)

    // cumulative l1 regularization as described in http://aclweb.org/anthology-new/P/P09/P09-1054.pdf
    val cumulative: PenaltyFunction = (w, x, u, q) => {
      val w1 = w.copy
      val q1 = q.copy
      x.activeKeysIterator.toList.foreach{ i =>
        val z = w(i)

        if(w1(i) gt 0.0)
          w1(i) = 0.0 max (w1(i) - (u + q1(i)))
        else if(w1(i) lt 0.0)
          w1(i) = 0.0 min (w1(i) + (u - q1(i)))

        q1(i) = q1(i) + w1(i) - z
      }

      (w1, q1)
    }
  }
}

object stochgraddesc extends SGDModule

/*

// in R
// set.seed(0);n <- 10000; x1 <- runif(n); x2 <- runif(n); e <- rnorm(n); y <- 10 * x1 + 5 * x2 + 10*e - 30; write.table(cbind(y,x1,x2),row.names=F, col.names=F, file='~/tmp/examples')

import scalaton.aggregate.stochgraddesc._

val examples = io.Source.fromFile("/home/elliot/tmp/examples").getLines.toSeq.map( _.trim.split(" ").map(_.toDouble).toSeq).map(p => sgd.example(p(0),p.drop(1) :+ util.Random.nextDouble))

val z = sgd.fit(glm.gaussian(learnrate.constant(0.01), penalty.cumulative, 0.01), sgd.weights(Seq(0,0,0)))(examples)

z._1

*/
