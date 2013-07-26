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

import scala.collection.JavaConversions._

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

    partial.groupByKey map { case (a, bs) => (a, bs reduce (_ |+| _)) }
  }

  def hcount[A : Manifest : WireFormat, L](dl: DList[A], group: String, f: A => L): DList[A] =
    dl.parallelDo((a: A, counters: Counters) => {
      counters.incrementCounter(group, f(a) toString, 1)
      a
    })

  def applyAll[A : Manifest : WireFormat, L](dl: DList[A], functions: List[DList[A] => DList[A]]): DList[A] = {
    @annotation.tailrec
    def loop(x: DList[A], fs: List[DList[A] => DList[A]]): DList[A] = fs match {
      case f :: rest => loop(f(x), rest)

      case Nil => x
    }

    loop(dl, functions)
  }

  def hcountN[A : Manifest : WireFormat](dl: DList[A], filters: List[(String, A => Boolean)]): DList[A] =
    applyAll(dl, filters.map{ case (group, f) => ((x: DList[A]) => hcount(x, group, f)) })

  def filterN[A : Manifest : WireFormat](dl: DList[A], filters: List[(String, A => Boolean)], emitCounters: Boolean = false): DList[A] = {
    val z = emitCounters ? hcountN(dl, filters) | dl

    applyAll(z, filters.map{ case (group, f) => ((x: DList[A]) => x filter f) })
  }

  def split[A : Manifest : WireFormat](dl: DList[A], filters: List[(String, A => Boolean)], emitCounters: Boolean = false) = {
    val z = emitCounters ? hcountN(dl, filters) | dl

    filters map { case (label, f) => (label, dl filter f) }
  }

  def counterKeys(sc: ScoobiConfiguration, removeSysCounters: Boolean = true): List[(String, String)] = {
    lazy val sysCounters = Set("org.apache.hadoop.mapreduce.lib.input.FileInputFormat$Counter", "org.apache.hadoop.mapred.Task$Counter", "org.apache.hadoop.mapreduce.lib.output.FileOutputFormat$Counter", "FileSystemCounters")

    for{
      g <- sc.counters.getGroupNames.toList
      c <- sc.counters.getGroup(g).iterator.map(_.getName)
      if removeSysCounters && !sysCounters.contains(g)
    } yield (g, c)
  }

  def readCounter(group: String, counter: String)(implicit sc: ScoobiConfiguration) =
    sc.counters.getGroup(group).findCounter(counter).getValue

}

object helpers extends HelperFunctions
