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

import scalaz._
import Scalaz._

import collection.immutable.TreeSet

import spire.math.Numeric

abstract class TopKByScore[A, B : Numeric, T] extends Monoid[Heap[(A,B)] @@ T]{
  type TopKData = Heap[(A,B)]

  val k: Int

  val numericB = implicitly[Numeric[B]]

  implicit val ordering: Order[(A, B)] = Order.orderBy(x => numericB.toDouble(x._2))

  val zero: TopKData @@ T = Tag(Heap.fromData(List[(A, B)]()))

  def append(x1: TopKData @@ T, x2: => TopKData @@ T): TopKData @@ T = {
    val x3 = (x1:TopKData) |+| (x2:TopKData)

    Tag(if(x3.size gt k) x3.drop(x3.size - k) else x3)
  }

  def insert(x: TopKData @@ T, ab: (A, B)): TopKData @@ T = {
    val x1 = x.insert(ab)

    Tag(if(x1.size gt k) x1.drop(1) else x1)
  }
}

object topk{
  def apply[A, B: Numeric, T](kk: Int) = new TopKByScore[A,B,T]{
    val k = kk
  }

  def fromData[A, B, T](as: Iterable[(A,B)])(implicit numericB: Numeric[B], topKMonoid: TopKByScore[A, B, T]): Heap[(A,B)] @@ T =
    as.foldLeft(topKMonoid.zero)((b, a) => topKMonoid.insert(b, a))
}
