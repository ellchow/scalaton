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

import scalaton.collection.immutable.Heap

import scalaz.{ Heap => _, Ordering => _, _ }
import Scalaz._

trait TopKModule{
  case class TopKData[A : Ordering](val k: Int, heap: Heap[A]){
    def isCompatibleWith(that: TopKData[A]) =
      this.k === that.k

    def insert(a: A): TopKData[A] = {
      val h = heap + a

      TopKData(k, if(h.size gt k) h.drop(1) else h)
    }

    def merge(that: TopKData[A]) = {
      require(isCompatibleWith(that))

      val h = this.heap merge that.heap

      TopKData(k, if(h.size gt k) h.drop(h.size - k) else h)
    }
  }

  object TopKData{
    def fromData[A : Ordering](k: Int)(as: Iterable[A]): TopKData[A] =
      as.foldLeft(TopKData(k, Heap.empty[A]))((t, a) => t.insert(a))
  }

  implicit def topKSemigroup[A]: Semigroup[TopKData[A]] = new Semigroup[TopKData[A]]{
    def append(t1: TopKData[A], t2: => TopKData[A]) = t1 merge t2
  }

}

object topk extends TopKModule
