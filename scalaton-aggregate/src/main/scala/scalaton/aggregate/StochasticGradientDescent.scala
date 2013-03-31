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

  type SGDFeatures = DenseVector[Double]
  type SGDWeights = DenseVector[Double]
  type SGDExample = (Double, SGDFeatures)
  type LearningRate = Double

  type UpdateFunction = (SGDWeights, SGDExample, LearningRate) => SGDWeights
  type GradientFunction = (DenseVector[Double], SGDExample) => DenseVector[Double]
  type PredictFunction = SGDWeights => SGDFeatures => Double

  def SGDExample(y: Double, x: Seq[Double]): SGDExample = (y, DenseVector((1.0 +: x) : _*))

  def SGDUpdate(g: GradientFunction): UpdateFunction = (w, example, alpha) => {
    val (y, x) = example

    w - (g(w, (y, x)) * alpha)
  }


  val LinearRegressionGradient: GradientFunction = { case (w, (y, x)) => x * ((w dot x) - y) }
  val LinearRegressionUpdate = SGDUpdate(LinearRegressionGradient)
  val LinearPredictor: PredictFunction = w => x => w dot x

  def SGDFit(update: UpdateFunction, initW: SGDWeights, alpha: LearningRate)(examples: Iterable[SGDExample]): SGDWeights =
    examples.foldLeft(initW)((w0, ex) => update(w0, ex, alpha))

  def combineSGDWeights(ws: Seq[SGDWeights]): SGDWeights =
    (ws reduce (_ + _)) / ws.size.toDouble
}

object sgd extends SGDModule

// set.seed(0);n <- 10000; x1 <- runif(n); x2 <- runif(n); e <- rnorm(n); y <- 10 * x1 + 2 * x2 + 10*e - 30; write.table(cbind(y,x1,x2),row.names=F, col.names=F, file='~/tmp/examples')

/*
import breeze.linalg._
import scalaton.aggregate.sgd._
val examples = io.Source.fromFile("/home/elliot/tmp/examples").getLines.toSeq.map( _.trim.split(" ").map(_.toDouble).toSeq).map(p => SGDExample(p(0),p.drop(1)))
SGDFit(LinearRegressionUpdate,DenseVector.zeros[Double](3), 0.01)(examples)
*/
