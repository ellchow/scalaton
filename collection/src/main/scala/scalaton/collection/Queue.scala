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


/* Banker's Queue implementation */
class Queue[+A] private (private val f: Stream[A], private val fn: Int, private val r: Stream[A], private val rn: Int)
    extends LinearSeq[A] with GenericTraversableTemplate[A, Queue] with LinearSeqLike[A, Queue[A]] {

  override val companion = Queue

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

  def enqueue[B >: A](elem: B) = rebalance(new Queue(f, fn, elem #:: (r: Stream[B]), rn + 1))

  def dequeue: (A, Queue[A]) = f match {
    case fh #:: ft =>
      (fh, rebalance(new Queue(ft, fn - 1, r, rn)))
    case _ => throw new NoSuchElementException("dequeue on empty queue")
  }

  override def iterator = (f ++ r.reverse).iterator

  private def rebalance[B](bq: Queue[B]) =
    if(bq.fn < bq.rn) {
      val m = rn - fn
      new Queue(bq.f ++ bq.r.reverse, bq.fn + bq.rn, Stream.empty, 0)
    } else {
      bq
    }

}

object Queue extends SeqFactory[Queue] {

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Queue[A]] = new GenericCanBuildFrom[A]

  def newBuilder[A]: mutable.Builder[A, Queue[A]] =
    new mutable.ListBuffer[A] mapResult { x => new Queue(x.reverse.toStream, 0, Stream.empty, 0) }

  override def empty[A] = new Queue[A](Stream.empty, 0, Stream.empty, 0)

  override def apply[A](xs: A*) = new Queue(xs.reverse.toStream, 0, Stream.empty, 0)

}
