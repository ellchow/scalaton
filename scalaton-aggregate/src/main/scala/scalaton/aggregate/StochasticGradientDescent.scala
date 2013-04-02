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

import collection.mutable

import breeze.linalg._

import scalaz._
import Scalaz._

trait SGDModule{

  type SGDFeatures = Vector[Double]
  type SGDWeights = Vector[Double]
  type SGDExample = (Double, SGDFeatures)
  type LearningRate = Double

  type UpdateFunction = (SGDWeights, SGDExample, LearningRate) => SGDWeights
  type GradientFunction = (Vector[Double], SGDExample) => Vector[Double]
  type PredictFunction = SGDWeights => SGDFeatures => Double

  def SGDExample(y: Double, x: Seq[Double]): SGDExample = (y, DenseVector((1.0 +: x) : _*))
  def SGDWeights(w: Seq[Double]): SGDWeights = DenseVector((0.0 +: w) : _*)

  def SGDUpdate(g: GradientFunction): UpdateFunction = (w, example, alpha) => {
    val (y, x) = example

    w - (g(w, (y, x)) * alpha)
  }

  def SGDFit(update: UpdateFunction, initW: SGDWeights, alpha: LearningRate = 0.01)(examples: Iterable[SGDExample]): SGDWeights =
    examples.foldLeft(initW)((w0, ex) => update(w0, ex, alpha))

  object glm{
    trait GLMOpt{
      def apply(initW: SGDWeights, alpha: LearningRate = 0.01) =
        SGDFit(update, initW, alpha) _

      val gradient: GradientFunction = { case (w, (y, x)) => x * (predict(w)(x) - y) }

      val update = SGDUpdate(gradient)

      val predict: PredictFunction
    }

    object gaussian extends GLMOpt {
      val predict: PredictFunction = w => x => w dot x
    }

    object bernoulli extends GLMOpt{
      val exp = breeze.generic.UFunc(math.exp _)

      def logisticFunction(x: Vector[Double], w: Vector[Double]) =
        1.0 / (1.0 + exp(- w dot x))

      val predict: PredictFunction = w => x => logisticFunction(x, w)
    }

  }

}

object sgd extends SGDModule

// set.seed(0);n <- 10000; x1 <- runif(n); x2 <- runif(n); e <- rnorm(n); y <- 10 * x1 + 2 * x2 + 10*e - 30; write.table(cbind(y,x1,x2),row.names=F, col.names=F, file='~/tmp/examples')

// set.seed(0);n<-10000;y <- as.integer(runif(n) < 0.1); x1 <- ifelse(y == 1, rnorm(n) + 1, rnorm(n)); x2 <- ifelse(y == 1, 2*rnorm(n) + 3, rnorm(n)); write.table(cbind(y,x1,x2),row.names=F, col.names=F, file='~/tmp/examples')

/*
import breeze.linalg._
import scalaton.aggregate.sgd._
val examples = io.Source.fromFile("/home/elliot/tmp/examples").getLines.toSeq.map( _.trim.split(" ").map(_.toDouble).toSeq).map(p => SGDExample(p(0),p.drop(1)))
glm.gaussian(SGDWeights(Seq(0,0)),alpha =  0.01)(examples)
glm.bernoulli(alpha =  0.01)(examples)
*/
