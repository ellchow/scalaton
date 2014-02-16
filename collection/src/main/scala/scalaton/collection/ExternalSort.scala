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

object ExternalSort {

  /* external sort implementation that sorts chunks of the incoming input and writes each to file; each chunk is read incrementally and merged together.
     currently requires elements to have a json codec for serialization to file
   */
  def sortBy[A : EncodeJson : DecodeJson, K : Ordering](xs: Iterator[A], groupSize: Int, tmp: File = mkTempDir())(key: A => K): Iterator[A] = {
    val chunks = xs.grouped(groupSize)

    val sortedChunks = chunks.map(_.sortBy(key))

    val handles = sortedChunks.zipWithIndex.map { case (chunk, i) =>
      val out = new File(tmp, i.toString)
      val w = new BufferedWriter(new FileWriter(out))

      try {
        chunk.foreach{ x => w.write(x.asJson.toString); w.newLine }
      } finally {
        w.close()
      }

      out
    }.toVector

    def pop(iter: Iterator[A]) = if (iter.hasNext) Some((iter.next, iter)) else None

    def merge(iters: Vector[Iterator[A]]) = {
      implicit def kbOrdering[B] =
        new Ordering[(K,B)] { def compare(x: (K,B), y: (K,B)) = implicitly[Ordering[K]].reverse.compare(x._1, y._1) }

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

    val is = handles.map{ f =>
      val inp = scala.io.Source.fromInputStream(new FileInputStream(f))

      inp.getLines.map{ ln =>
        ln.decodeEither[A].fold(s => throw new Exception(s"failed to deserialize ($s)"), identity)
      }
    }

    merge(is)
  }

  def sort[A : EncodeJson : DecodeJson : Ordering](xs: Iterator[A], groupSize: Int, tmp: File = mkTempDir()): Iterator[A] =
    sortBy(xs, groupSize, tmp)(identity)

}
