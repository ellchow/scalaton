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

package scalaton.doo

import scalaton.util._
import scalaton.util.hashing._
import scalaton.util.hashing32._

import com.nicta.scoobi.Scoobi._

import scalaz.{DList => _, _}
import Scalaz._

trait SamplingFunctions {

  def sample[A : Manifest : WireFormat](dl: DList[A], rate: Double, seed: Int = 0)(implicit hashable: Hashable[A,Bits32]) = {
    val n = (Int.MaxValue * rate) toInt

    dl.filter{ x => math.abs(hash[A,Bits32](x)) < n }
  }

  def sampleBy[A : Manifest : WireFormat, B : Manifest : WireFormat](dl: DList[(A,B)], rate: Double, seed: Int = 0)(implicit hashable: Hashable[A,Bits32]) = {
    val n = (Int.MaxValue * rate)

    dl.filter{ case (a, _) => math.abs(hash[A, Bits32](a)) < n }
  }

  def limit[A : Manifest : WireFormat](dl: DList[A], limit: Int) = {
    def limitFun = new DoFn[A, A] {
      private var count = limit;

      def setup() {}

      def process(input: A, emitter: Emitter[A]) {
        if (count > 0) {
          count = count - 1
          emitter.emit(input)
        }
      }

      def cleanup(emitter: Emitter[A]) {}
    }

    dl parallelDo limitFun
  }

  def partitionAtRandom[A : Manifest : WireFormat](dl: DList[A], n: Int, seed: Int = 0): DList[A] = {
    require(n > 0, "number of partitions must be > 0")

    def addRandomFun = new DoFn[A, ((Int, Int), A)] {
      val rand1 = new util.Random(seed)
      val rand2 = new util.Random(seed + 1)

      def setup() {}

      def process(input: A, emitter: Emitter[((Int, Int), A)]) {
        emitter.emit(((rand1 nextInt n, rand2 nextInt), input))
      }

      def cleanup(emitter: Emitter[((Int, Int), A)]) {}

    }

    (dl parallelDo addRandomFun).groupByKeyWith(grouping.secondarySort[Int,Int]).mapFlatten(_._2)
  }

}

object sampling extends SamplingFunctions
