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
    type PenaltyFunction = (Weights, TotalPenalty, ActualPenalty) => (Weights, ActualPenalty)
    type GradientFunction = (Weights, Example) => Gradient
  }
  import sgdtypes._

  object sgd{
    def example(y: Double, x: Seq[Double]): Example = (y, DenseVector((1.0 +: x) : _*))

    def example(y: Double, x: Seq[(Int,Double)], size: Int): Example = {
      val (indices, values) = x.foldLeft((SVector[Int](0),SVector[Double](1.0))){
        case ((i, v), (ii, vv)) => (i :+ (ii + 1), v :+ vv)
      }
      val z = new SparseVector(indices.toArray, values.toArray, size + 1)
      (y, z)
    }

    def weights(w: Seq[Double]): Weights = DenseVector((0.0 +: w) : _*)

    def weights(w: Seq[(Int,Double)], size: Int): Weights = {
      val (indices, values) = w.foldLeft((SVector[Int](0),SVector[Double](0.0))){
        case ((i, v), (ii, vv)) => (i :+ (ii + 1), v :+ vv)
      }

      val z = new SparseVector(indices.toArray, values.toArray, size + 1)
      z
    }

    def update(gradient: GradientFunction)(learningRate: LearningRateFunction, penalize: PenaltyFunction, c: RegularizationParameter)(w0: Weights, u0: TotalPenalty, q0: ActualPenalty, k0: NumExamples)(ex: Example) = {
      val eta = learningRate(k0)
      val k1 = k0 + 1
      val u1 = u0 + c * eta
      val (y, x) = ex

      val (w1, q1) = penalize(w0 - (gradient(w0, (y, x)) * eta),
                              u1,
                              q0)

      (w1, u1, q1, k1)
    }

    def fit(f: (Weights, TotalPenalty, ActualPenalty, NumExamples) => Example => (Weights, TotalPenalty, ActualPenalty, NumExamples), init: Weights)(examples: Iterable[Example]) = {
      examples.foldLeft((init, 0.0: TotalPenalty, SparseVector.zeros[Double](init.size) : ActualPenalty, 0L: NumExamples)){
        case ((w, u, q, k), ex) =>
          f(w, u, q, k)(ex)
      }
    }
  }

  object glm{
    val gaussian = sgd.update{ case (w, (y, x)) => x * ((w dot x) - y) } _

    val bernoulli = sgd.update{ case (w, (y, x)) => x * (logistic(w dot x) - y) } _

    def logistic(z: Double) = 1.0 / (1.0 + math.exp(- z))
  }

  object learnrate{
    def constant(c: Double): LearningRateFunction = _ => c
  }

  object penalty{
    val zero: PenaltyFunction = (w, u, q) => (w, q)

    // cumulative l1 regularization as described in http://aclweb.org/anthology-new/P/P09/P09-1054.pdf
    val cumulative: PenaltyFunction = (w, u, q) => {
      val w1 = w.copy
      val q1 = q.copy
      w1.activeKeysIterator.toList.foreach{ i =>
        val z = w(i)

        if(w1(i) gt 0.0){
          w1(i) = 0.0 max (w1(i) - (u + q1(i)))
        }else if(w1(i) lt 0.0){
          w1(i) = 0.0 min (w1(i) + (u - q1(i)))
        }
        q1(i) = q1(i) + w1(i) - z
      }

      (w1, q1)
    }
  }


}

object stochgraddesc extends SGDModule

