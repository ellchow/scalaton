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

import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.core.Reduction

import scalaz.{DList => _, _}
import Scalaz._

trait HelperFunctions {

  def parallelFold[A : Manifest : WireFormat, B : Manifest : WireFormat](dl: DList[A], init: B)(f: (B, A) => B) = {
    def foldFun = new DoFn[A, B]{
      private var b = init

      def setup() {}

      def process(a: A, emitter: Emitter[B]){
        b = f(b, a)
      }

      def cleanup(emitter: Emitter[B]) {
        emitter emit b
      }

    }

    dl parallelDo foldFun
  }

  def parallelFoldMonoid[A : Manifest : WireFormat, B : Manifest : WireFormat : Monoid](dl: DList[A])(f: (B, A) => B) =
    parallelFold(dl, implicitly[Monoid[B]].zero)(f)

  def groupByKeyThenCombine[A : Manifest : WireFormat : Grouping, B : Manifest : WireFormat : Semigroup](dl: DList[(A,B)]): DList[(A, B)] = {

    def combineFun = new DoFn[(A, B), (A, B)]{
      private var combined = Map[A,B]()

      def setup() {}

      def process(ab: (A, B), emitter: Emitter[(A, B)]){
        combined = combined |+| Map(ab)
      }

      def cleanup(emitter: Emitter[(A, B)]) {
        combined foreach emitter.emit
      }

    }

    (dl parallelDo combineFun).groupByKey.map{ case (a, bs) => (a, bs reduce (_ |+| _))}
  }
}

object helpers extends HelperFunctions
