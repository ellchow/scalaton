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
import scala.collection.immutable._
import scala.collection.LinearSeqLike
import scala.collection.mutable

class BankersQueue[+A] private (private val f: Stream[A], private val fn: Int, private val r: Stream[A], private val rn: Int)
    extends LinearSeq[A] with GenericTraversableTemplate[A, BankersQueue] with LinearSeqLike[A, BankersQueue[A]] {
  override val companion = BankersQueue

  def length = fn + rn

  def apply(i: Int) = {
    if (i < fn) {
      f(i)
    } else if (i < fn + rn) {
      r(rn - (i - fn) - 1)
    } else {
      throw new NoSuchElementException("index out of range: " + i)
    }
  }

  def enqueue[B >: A](elem: B) = rebalance(new BankersQueue(f, fn, elem #:: r, rn + 1))

  def dequeue: (A, BankersQueue[A]) = f match {
    case fh #:: ft => (fh, rebalance(new BankersQueue(ft, fn - 1, r, rn)))
    case _ => throw new NoSuchElementException("dequeue on empty queue")
  }

  override def iterator = (f ++ r.reverse).iterator

  private def rebalance(q) = if(fn < rn) {
    val m = rn - fn
    new BankersQueue(f.)
  } else {
    this
  }

}


object BankersQueue extends SeqFactory[BankersQueue] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, BankersQueue[A]] = new GenericCanBuildFrom[A]

  def newBuilder[A]: mutable.Builder[A, BankersQueue[A]] =
    new mutable.ListBuffer[A] mapResult { x => new BankersQueue(x.reverse.toStream, 0, Stream.empty, 0) }

  override def empty[A] = new BankersQueue[A](Stream.empty, 0, Stream.empty, 0)

  override def apply[A](xs: A*) = new BankersQueue(xs.reverse.toStream, 0, Stream.empty, 0)
}
