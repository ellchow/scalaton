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
