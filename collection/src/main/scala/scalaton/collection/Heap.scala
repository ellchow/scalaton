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

package scalaton.collection.immutable

import scala.collection._
import scala.collection.generic._
// import scala.collection.immutable._
import scala.collection.mutable

abstract class Heap[A](implicit ordering: Ordering[A]) extends Iterable[A] with GenericOrderedTraversableTemplate[A, Heap] with IterableLike[A, Heap[A]] {

  override val companion = Heap

  def +(elem: A): Heap[A]

  def min(elem: A): A

  def removeMin: (A, Heap[A])

  def merge(that: Heap[A]): Heap[A]

  // override def iterator

}


object Heap extends GenericCompanion[Heap] {

}
