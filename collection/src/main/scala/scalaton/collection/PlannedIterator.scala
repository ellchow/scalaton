/*
 Copyright 2014 Elliot Chow

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

package scalaton.collection

import scala.util.Try
import scala.concurrent._, duration._
import java.io._
import argonaut._, Argonaut._

trait PlannedIterator[A,B] { self =>
  import Join._

  protected[collection] def underlying: Iterator[A]
  protected def f: Iterator[A] => Iterator[B]
  protected def completionHook(): Unit
  protected[collection] def applied = f(underlying)

  def apply[C](g: Iterator[B] => Iterator[C]) = new PlannedIterator[A,C] {
    protected[collection] def underlying: Iterator[A] = self.underlying
    protected def f = self.f.andThen(g)
    protected def completionHook(): Unit = self.completionHook
  }
  def convert[C](g: Iterator[B] => C): Try[C] = {
    val c = Try(g(applied))
    completionHook()
    c
  }

  def run = foldLeft(Unit)((c,b) => Unit)

  // wrappers

  def buffered = apply(_.buffered)

  def collect[C](pf: PartialFunction[B,C]) = apply(_.collect(pf))

  def contains(b: B) = exists(_ == b)

  def count(pred: B => Boolean) = convert(_.count(pred))

  def drop(n: Int) = apply(_.drop(n))

  def dropWhile(pred: B => Boolean) = apply(_.dropWhile(pred))

  def exists(pred: B => Boolean) = convert(_.exists(pred))

  def flatMap[C](g: B => scala.collection.GenTraversableOnce[C]) =
    apply(_.flatMap(g))

  def filter(pred: B => Boolean) = apply(_.filter(pred))

  def find(pred: B => Boolean) = convert(_.find(pred))

  def foldLeft[C](c0: C)(g: (C,B) => C): Try[C] =
    convert(_.foldLeft(c0)(g))

  def groupBy[K : Ordering](key: B => K) = map(b => (key(b), b)).apply(_.groupByKey)

  def grouped(n: Int) = apply(_.grouped(n))

  def map[C](g: B => C) = apply(_.map(g))

  def max(implicit ord: Ordering[B]) = convert(_.max)

  def maxBy[C](f: B => C)(implicit ord: Ordering[C]) = convert(_.maxBy(f))

  def min(implicit ord: Ordering[B]) = convert(_.min)

  def minBy[C](f: B => C)(implicit ord: Ordering[C]) = convert(_.minBy(f))

  def reduce(g: (B,B) => B) = convert(_.reduce(g))

  def sliding(size: Int, step: Int = 1) = apply(_.sliding(size, step))

  def size = convert(_.size)

  def take(n: Int) = apply(_.take(n))

  def thru(g: B => Unit) = map{ b => g(b) ; b }

  def takeWhile(pred: B => Boolean) = apply(_.takeWhile(pred))

  def zipWithIndex = apply(_.zipWithIndex)

  def zip[A1,B1](other: PlannedIterator[A1,B1]) = new PlannedIterator[A,(B,B1)] {
    protected[collection] def underlying: Iterator[A] = self.underlying
    protected def f = (as: Iterator[A]) => self.f(as).zip(other.f(other.underlying))
    protected def completionHook(): Unit = {
      self.completionHook
      other.completionHook
    }
  }

  def futured(implicit execContext: ExecutionContext) = apply(_.map(b => future(b)))

  def addCompletionHook(onComplete: =>Unit) = new PlannedIterator[A,B] {
    protected[collection] def underlying: Iterator[A] = self.underlying
    protected def f = self.f
    protected def completionHook(): Unit = {
      self.completionHook
      onComplete
    }
  }
}

trait PlannedIteratorFunctions {
  def plannedIterator[A](iterator: Iterator[A], onComplete: =>Unit = Unit) = new PlannedIterator[A,A] {
    protected[collection] def underlying: Iterator[A] = iterator
    protected def f = identity _
    protected def completionHook(): Unit = onComplete
  }

  def resourceIterator[A,R <: Closeable](init: =>(R, Iterator[A])) = {
    val (r, iterator) = init
    plannedIterator(iterator, r.close())
  }

  def linesIterator(input: =>InputStream) = {
    val r = input
    resourceIterator((r, scala.io.Source.fromInputStream(r).getLines))
  }

  implicit def iteratorToPlannedIterator[A](iterator: Iterator[A]) = plannedIterator(iterator)

  implicit class IteratorPlanned[A](iterator: Iterator[A]) {
    def planned = plannedIterator(iterator)
  }

  implicit class PlannedIteratorOps[A,B](p: PlannedIterator[A,B]) {
    def to(out: OutputStream, ser: B => Array[Byte], delim: Array[Byte], onWriteError: PartialFunction[Throwable,Unit]): PlannedIterator[A,Unit] =
      p.addCompletionHook(out.close).map{ b =>
        try {
          out.write(ser(b))
          out.write(delim)
        } catch {
          onWriteError
        }
      }

    def to(out: OutputStream, ser: B => String, delim: String, onWriteError: PartialFunction[Throwable,Unit]): PlannedIterator[A,Unit] =
      to(out, ser.andThen(_.getBytes), delim.getBytes, onWriteError)

    def to(out: OutputStream, delim: String = "\n", onWriteError: PartialFunction[Throwable,Unit] = { case t => throw t })(implicit e: EncodeJson[B]): PlannedIterator[A,Unit] =
      to(out, (_:B).asJson.toString.getBytes, delim.getBytes, onWriteError)

    def checkpoint(out: OutputStream, in: =>InputStream, onWriteError: PartialFunction[Throwable,Unit] = { case t => throw t })(implicit ee: EncodeJson[B], ed: DecodeJson[B]): Try[PlannedIterator[String,B]] = {
      to(out, "\n", onWriteError).run.map{ _ =>
        val i = in
        plannedIterator(scala.io.Source.fromInputStream(i).getLines).addCompletionHook(i.close).map(s => s.decodeOption[B] match {
          case Some(b) => b
          case None => throw new IOException("unable to decode json $b")
        })
      }
    }
  }

  implicit class KeyedPlannedIteratorOps[K,A,B](p: PlannedIterator[A,(K,B)]) {
    import Join._

    def groupByKey(implicit ord: Ordering[K]) = p.apply(_.groupByKey)

    def coGroup[A1,B1](other: PlannedIterator[A1,(K,B1)])(implicit ord: Ordering[K]) =
      p.apply(_.coGroup(other.applied))

    def innerJoin[A1,B1](other: PlannedIterator[A1,(K,B1)])(implicit ord: Ordering[K]) =
      p.apply(_.innerJoin(other.applied))

    def fullOuterJoin[A1,B1](other: PlannedIterator[A1,(K,B1)])(implicit ord: Ordering[K]) =
      p.apply(_.fullOuterJoin(other.applied))

    def rightOuterJoin[A1,B1](other: PlannedIterator[A1,(K,B1)])(implicit ord: Ordering[K]) =
      p.apply(_.rightOuterJoin(other.applied))

    def leftOuterJoin[A1,B1](other: PlannedIterator[A1,(K,B1)])(implicit ord: Ordering[K]) =
      p.apply(_.leftOuterJoin(other.applied))
  }

  implicit class PlannedFutureIteratorOps[A,B](p: PlannedIterator[A,Future[B]]) {
    def unfutured(n: Int = 2, timeout: Duration = Duration.Inf)(implicit execContext: ExecutionContext) =
      p.apply(_.grouped(n).flatMap(bs => Await.result(Future.sequence(bs), timeout)))
  }
}
