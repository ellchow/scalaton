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

import scala.collection.generic._

class BiMap[A,B] private (forward: Map[A,B], backward: Map[B,A]) extends Iterable[(A,B)] {

  def +(kv: (A, B)): BiMap[A,B] = {
    val bkd =  get(kv._1).map(b => backward - b).getOrElse(backward)
    new BiMap(forward + kv, bkd + kv.swap)
  }

  def -(a: A): BiMap[A,B] = get(a).map{ b =>
    new BiMap(forward - a, backward - b)
  }.getOrElse(this)

  def get(a: A): Option[B] = forward.get(a)

  def apply(a: A): B = get(a).get

  def contains(a: A): Boolean = get(a).nonEmpty

  def flip: BiMap[B,A] = new BiMap(backward, forward)

  def iterator = forward.iterator

}

object BiMap {
  def empty[A,B] = new BiMap(Map.empty[A,B], Map.empty[B,A])

  def apply[A,B](kvs: Iterable[(A,B)]): BiMap[A,B] = kvs.foldLeft(empty[A,B])((bm,kv) => bm + kv)

  def apply[A,B](kvs: (A,B)*): BiMap[A,B] = apply(kvs)
}
