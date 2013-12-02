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

package scalaton.aggregate.hashed

import scala.collection.BitSet
import scala.collection.mutable.{BitSet => MBitSet}
import com.googlecode.javaewah.{EWAHCompressedBitmap => CompressedBitSet}

import scala.util.{Random => SRandom}

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._

import org.scalacheck._

import scalaz.{Tag => ZTag, _}
import Scalaz._

import scalaton.util.hashing128._
import scalaton.aggregate.hashed.hyperloglog._
import scalaton.aggregate.hashed.hyperloglog.implicits._

class HyperLogLogSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  behavior of "a hyper log log estimator"

  it should "track cardinality with reasonable error rate" in {

    for{ b <- Seq(7, 8, 9) }
    yield {
      (1 to 10000).foldLeft((List[Double](), DenseHyperLogLog.empty(b, 0L))){
        case ((lst, d), x) =>
          val dd = d.insert(x)
          val e = dd.size
          val err = math.abs(x - e).toDouble / x

          (err :: lst, dd)
      }
    }
  }
}

