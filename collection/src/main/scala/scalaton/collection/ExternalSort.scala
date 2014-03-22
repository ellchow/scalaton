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

package scalaton.collection

import argonaut._, Argonaut._
import java.io._
import scala.collection.mutable.PriorityQueue
import scalaton.util._
import scalaton.util.paths._
import scala.util.{ Try, Success, Failure }

object ExternalSort {

  /** external sort implementation that sorts chunks of the incoming input and writes each to file; each chunk is read incrementally and merged together.
     currently requires elements to have a json codec for serialization to file
  */
  def sortBy[A : EncodeJson : DecodeJson, K : Ordering](xs: Iterator[A], groupSize: Int, tmp: Path)(key: A => K): Try[PlannedIterator[A,A]] = {
    val chunks = plannedIterator(xs).grouped(groupSize)
    val sortedChunks = chunks.map(_.sortBy(key))

    val handlesTry = sortedChunks.zipWithIndex.map{ case (chunk, i) =>
      val out = tmp / i.toString
      val w = new PrintStream(new java.util.zip.GZIPOutputStream(new FileOutputStream(out.file)))

      try {
        chunk.foreach{ x => w.println(x.asJson.toString) }
      } finally {
        w.close()
      }

      out
    }.convert(_.toVector)

    handlesTry.map{ handles =>
      val inputs = handles.map{ p =>
        new java.util.zip.GZIPInputStream(new FileInputStream(p.file))
      }
      val iterators = inputs.map{ input =>
        scala.io.Source.fromInputStream(input).getLines.map{ ln =>
          ln.decodeEither[A].fold({ s => input.close(); throw new Exception(s"failed to deserialize ($s)") }, identity)
        }
      }
      plannedIterator(merge(iterators)(key)).addCompletionHook{
        inputs.foreach{ i => i.close }
        handles.foreach(f => Filesystem.delete(f))
      }
    }
  }

  def sortBy[A : EncodeJson : DecodeJson, K : Ordering](xs: Iterator[A], groupSize: Int)(key: A => K)(implicit osSpecific: OSSpecific): Try[PlannedIterator[A,A]] =
    sortBy(xs, groupSize, Filesystem.mkTempDir())(key)

  def merge[A, K : Ordering](iters: Vector[Iterator[A]])(key: A => K) = {
    implicit def kbOrdering[B] =
      new Ordering[(K,B)] { def compare(x: (K,B), y: (K,B)) = implicitly[Ordering[K]].reverse.compare(x._1, y._1) }

    def pop(iter: Iterator[A]) = if (iter.hasNext) Some((iter.next, iter)) else None

    new Iterator[A] {
      private val q = PriorityQueue.empty[(K, (A, Iterator[A]))]
      iters.foreach(i => pop(i).foreach{ case (a, rest) => q.enqueue((key(a), (a, rest))) })

      def hasNext = q.nonEmpty

      def next = {
        val (_, (a, i)) = q.dequeue()
        pop(i).foreach{ case (a, rest) => q.enqueue((key(a), (a, rest))) }
        a
      }
    }
  }

  def sorted[A : EncodeJson : DecodeJson : Ordering](xs: Iterator[A], groupSize: Int, tmp: Path): Try[PlannedIterator[A,A]] =
    sortBy(xs, groupSize, tmp)(identity)

  def sorted[A : EncodeJson : DecodeJson : Ordering](xs: Iterator[A], groupSize: Int)(implicit osSpecific: OSSpecific): Try[PlannedIterator[A,A]] =
    sorted(xs, groupSize, Filesystem.mkTempDir())

}
