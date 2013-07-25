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

  def groupByKeyThenCombine[A : Manifest : WireFormat : Grouping, B : Manifest : WireFormat](dl: DList[(A,B)])(implicit semigroupB: Semigroup[B]): DList[(A, B)] = {
    val partial = parallelFold(dl, Map[A,B]())( (combined, ab) => combined |+| Map(ab) ) mapFlatten ( _ toSeq )

    partial.groupByKey map { case (a, bs) => (a, bs reduce (_ |+| _))}
  }

  def filterWithCounters[A : Manifest : WireFormat](dl: DList[A], filters: List[(String, A => Boolean)]) = {
    @annotation.tailrec
    def loop(x: DList[A], fs: List[(String, A => Boolean)]): DList[A] = fs match {
      case (group, f) :: rest => loop(x.parallelDo((a: A, counters: Counters) => {
        counters.incrementCounter(group, f(a) toString, 1)
        a
      }).filter(f), rest)

      case Nil => x
    }

    loop(dl, filters)
  }
}

object helpers extends HelperFunctions
