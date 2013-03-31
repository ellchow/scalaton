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

import scalaz.{DList => _, _}
import Scalaz._


trait HelperFunctions {

  def accumulate[A : Manifest : WireFormat, B : Manifest : WireFormat : Monoid](dl: DList[A])(f: (B, A) => B): DObject[B] = {
    def accumulateFun = new DoFn[A, B] {
      private var b = implicitly[Monoid[B]].zero

      def setup() {}

      def process(a: A, emitter: Emitter[B]){
        b = f(b, a)
      }

      def cleanup(emitter: Emitter[B]) {
        emitter emit b
      }
    }

    (dl parallelDo accumulateFun).materialise map (_ reduce (_ |+| _))
  }

}


object helpers extends HelperFunctions
