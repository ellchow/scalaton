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

import org.specs2.mutable._

import scalaton.aggregate.moments._

import scalaz._
import Scalaz._

class MomentsSpec extends Specification{

  def relativeError(x: Double, correct: Double) = math.abs((x - correct) / correct)

  "Moments object" should {
    "count" in {
      (Moments.from((1 to 5).map(_ toDouble)).n === 5) must beTrue

      (Moments.from((1 to 100).map(_ toDouble)).n === 100) must beTrue
    }

    "compute mean" in {
      relativeError(Moments.from((1 to 100).map(_ toDouble)).mean, 50.5) must beLessThan(0.001)

      relativeError(Moments.from(Seq(0.101,-0.172,0.183,0.045,0.408,-1.156,-0.064,0.282,1.145,-1.042,1.595,-0.212,-0.606,-0.464,0.64,-0.957,0.773,0.274,0.631,-0.526)).mean, 0.0439) must beLessThan(0.001)
    }

    "compute variance" in {
      relativeError(Moments.from((1 to 100).map(_ toDouble)).variance, 841.667) must beLessThan(0.05)

      relativeError(Moments.from(Seq(0.101,-0.172,0.183,0.045,0.408,-1.156,-0.064,0.282,1.145,-1.042,1.595,-0.212,-0.606,-0.464,0.64,-0.957,0.773,0.274,0.631,-0.526)).variance, 0.5191766) must beLessThan(0.05)
    }

    "compute skewness" in {
      relativeError(Moments.from((1 to 100).map(_ toDouble)).skewness + 1, 1) must beLessThan(0.001)

      relativeError(Moments.from(Seq(0.101,-0.172,0.183,0.045,0.408,-1.156,-0.064,0.282,1.145,-1.042,1.595,-0.212,-0.606,-0.464,0.64,-0.957,0.773,0.274,0.631,-0.526)).skewness, 0.189957) must beLessThan(0.001)
    }

    "compute kurtosis" in {
      relativeError(Moments.from((1 to 100).map(_ toDouble)).kurtosis, -1.20024) must beLessThan(0.001)

      relativeError(Moments.from(Seq(0.101,-0.172,0.183,0.045,0.408,-1.156,-0.064,0.282,1.145,-1.042,1.595,-0.212,-0.606,-0.464,0.64,-0.957,0.773,0.274,0.631,-0.526)).kurtosis, -0.403229) must beLessThan(0.001)
    }

  }
}
