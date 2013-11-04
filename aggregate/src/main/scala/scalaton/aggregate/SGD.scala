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

import la4s._

trait SGDModule{
  type Features = Vec
  type Target = Double
  type Example = (Target, Features)
  type Weights = Vec
  type Gradient = Vec
  type TotalPenalty = Double
  type ActualPenalty = Vec
  type NumExamples = Long
  type RegularizationParameter = Double
  type LearningRateFunction = NumExamples => Double
  type RegularizationFunction = (Weights, Features, TotalPenalty, ActualPenalty) => (Weights, ActualPenalty)
  type GradientFunction = (Weights, Example) => Gradient

  def weights(ws: Seq[Double]) = Vec.dense((1.0 +: ws): _*)

  def weights(ws: Seq[(Int, Double)], size: Int) =
    Vec.sparse(size + 1, ((0, 0.0) +: ws.view.map{ case (i, v) => (i + 1, v) }): _*)

  def example(y: Double, x: Seq[Double]) = (y, weights(x))

  def example(y: Double, x: Seq[(Int, Double)], size: Int) = (y, weights(x, size))

  def update(gradient: GradientFunction)(learningRate: LearningRateFunction, penalize: RegularizationFunction, c: RegularizationParameter)(w0: Weights, u0: TotalPenalty, q0: ActualPenalty, k0: NumExamples)(ex: Example) = {
    val eta = learningRate(k0)
    val k1 = k0 + 1
    val u1 = u0 + c * eta
    val (y, x) = ex
    val g = gradient(w0, (y, x))
    val (w1, q1) = penalize(w0 - (g :* eta), x, u1, q0)

    (w1, u1, q1, k1)
  }

  def fit(f: (Weights, TotalPenalty, ActualPenalty, NumExamples) => Example => (Weights, TotalPenalty, ActualPenalty, NumExamples), init: Weights)(examples: Iterable[Example]) = {
    examples.foldLeft((init, 0.0: TotalPenalty, init.blank, 0L: NumExamples)){
        case ((w, u, q, k), ex) => f(w, u, q, k)(ex)
      }
  }

  object glm{
    val gaussian = update { case (w, (y, x)) =>
                            val err = (w dot x) - y

                            err *: x
                          } _

    val bernoulli = update{ case (w, (y, x)) =>
                            val err = logistic(w dot x) - y

                            err *: x
                          } _

    val poisson = update{ case (w, (y, x)) =>
                          val err = math.exp(w dot x) - y

                          err *: x
                        } _

    def logistic(z: Double) = 1.0 / (1.0 + math.exp(-z))
  }

  object learnrate{
    def constant(c: Double): LearningRateFunction = _ => c

    def exponential(n0: Double, alpha: Double, N: Int): LearningRateFunction =
      k => n0 * math.pow(alpha, -k / N)
  }

  object penalty{
    val zero: RegularizationFunction = (w, x, u, q) => (w, q)

    /* cumulative l1 regularization as described in http://aclweb.org/anthology-new/P/P09/P09-1054.pdf */
    // val cumulative: RegularizationFunction = (w, x, u, q) => {
      // val w1 = w // copy
      // val q1 = q // copy

      // val f = new VectorProcedure{
      //   def apply(i: Int, value: Double){
      //     val z = w.get(i)

      //     if(value =/= 0.0){
      //       if(w1.get(i) gt 0.0)
      //         w1.set(i, 0.0 max (w1.get(i) - (u + q1.get(i))))
      //       else
      //         w1.set(i, 0.0 min (w1.get(i) + (u - q1.get(i))))

      //       q1.set(i, q1.get(i) + w1.get(i) - z)
      //     }
      //   }
      // }

      // x each f

      // (w1, q1)
    // }
  }

}

object sgd extends SGDModule
