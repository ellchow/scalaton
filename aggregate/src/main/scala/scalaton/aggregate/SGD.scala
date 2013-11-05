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

import scalaton.util.tag._

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
  type LearnRate = Double
  type LearnRateFunction = NumExamples => LearnRate
  type RegularizationFunction = (Weights, NumExamples, LearnRate) => Weights
  type GradientFunction = (Weights, Example) => Gradient

  def weights(ws: Seq[Double]) = Vec.dense((1.0 +: ws): _*)

  def weights(ws: Seq[(Int, Double)], size: Int) =
    Vec.sparse(size + 1, ((0, 1.0) +: ws.view.map{ case (i, v) => (i + 1, v) }): _*)

  def example(y: Double, x: Seq[Double]) = (y, weights(x))

  def example(y: Double, x: Seq[(Int, Double)], size: Int) = (y, weights(x, size))

  def update[C](gradient: GradientFunction)(learningRate: LearnRateFunction, penalize: RegularizationFunction)(w0: Weights, k0: NumExamples)(ex: Example) = {
    val eta = learningRate(k0)
    val k1 = k0 + 1
    val (y, x) = ex

    val g = gradient(w0, ex)
    val w1 = penalize(w0 - (g :* eta), k1, eta)

    (w1, k1)
  }

  def fit(f: (Weights, NumExamples) => Example => (Weights, NumExamples), init: Weights)(examples: Iterable[Example]) =
    examples.foldLeft((init, 0L: NumExamples)){ case ((w, k), ex) => f(w, k)(ex) }

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
    def constant(c: Double): LearnRateFunction = _ => c

    def exponential(n0: Double, alpha: Double, N: Int): LearnRateFunction =
      k => n0 * math.pow(alpha, -k / N)

    def inverse(c: Double): LearnRateFunction = k => c / k
  }

  object penalty{
    val none: RegularizationFunction = (w, n, eta) => w

    def every(interval: Int)(r: RegularizationFunction): RegularizationFunction = {
      require(interval > 0)

      (w, k, eta) => {
        val res = if((k % interval) == (interval - 1)) r(w, k, eta) else w
        res
      }
    }

    /** http://dl.acm.org/ft_gateway.cfm?id=1577097&ftid=774577&dwn=1&CFID=258767130&CFTOKEN=34455949 */
    def truncated(threshold: Double = 5, gravity: Double = 0.1, interval: Int = 10, f: NumExamples => Double = x => math.log(x)): RegularizationFunction = {
      require(threshold > 0.0)
      require(gravity > 0.0)

      every(interval){ (w, k, eta) =>
        val alpha = eta * gravity * f(k)

        val res = w.mapNonzero{ ww =>
          if(ww > 0.0 && ww < threshold){
            (ww - alpha) max 0
          }else if (ww < 0.0 && ww > -threshold){
            (ww + alpha) min 0
          }else{
            ww
          }
        }

        res
      }
    }
  }
}

object sgd extends SGDModule

// in R
// set.seed(0);n <- 10000; x1 <- runif(n); x2 <- runif(n); e <- rnorm(n); y <- 10 * x1 + 5 * x2 + 10*e - 30; write.table(cbind(y,x1,x2),row.names=F, col.names=F, file='~/tmp/examples')
/*
import scalaton.aggregate._
val examples = io.Source.fromFile("/Users/ellchow/tmp/examples").getLines.toSeq.map( _.trim.split(" ").map(_.toDouble).toSeq).map(p => sgd.example(p(0),p.drop(1) :+ util.Random.nextDouble))
val z = sgd.fit(sgd.glm.gaussian(sgd.learnrate.constant(0.01), sgd.penalty.truncated()), sgd.weights(Seq(0,0,0)))(examples ++ examples ++ examples ++ examples)

 val z = sgd.fit(sgd.glm.gaussian(sgd.learnrate.constant(0.01), sgd.penalty.none), sgd.weights(Seq(0,0)))(examples ++ examples)
*/
