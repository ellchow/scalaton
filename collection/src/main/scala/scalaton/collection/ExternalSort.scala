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
import scalaz.{ Ordering => _ , _ }, Scalaz._
import scalaz.stream._, Process._
import scalaz.concurrent._

object ExternalSort {

  def sort[A : EncodeJson : DecodeJson : Ordering](xs: Process[Task,A], groupSize: Int, tmp: Path): Process[Task,A] =
    sortBy(xs, groupSize, tmp)(identity)

  def sort[A : EncodeJson : DecodeJson : Ordering](xs: Process[Task,A], groupSize: Int)(implicit osSpecific: OSSpecific): Process[Task,A] =
    sort(xs, groupSize, Filesystem.mkTempDir())

  def sort[A : EncodeJson : DecodeJson : Ordering](xs: Iterable[A], groupSize: Int, tmp: Path): Process[Task,A] =
    sort(emitAll(xs.toSeq): Process[Task,A], groupSize, tmp)

  def sort[A : EncodeJson : DecodeJson : Ordering](xs: Iterable[A], groupSize: Int)(implicit osSpecific: OSSpecific): Process[Task,A] =
    sort(xs, groupSize, Filesystem.mkTempDir())

  def sortBy[A : EncodeJson : DecodeJson, K : Ordering](xs: Process[Task,A], groupSize: Int)(key: A => K)(implicit osSpecific: OSSpecific): Process[Task,A] =
    sortBy(xs, groupSize, Filesystem.mkTempDir())(key)

  def sortBy[A : EncodeJson : DecodeJson, K : Ordering](xs: Iterable[A], groupSize: Int, tmp: Path)(key: A => K)(implicit osSpecific: OSSpecific): Process[Task,A] =
    sortBy(emitAll(xs.toSeq): Process[Task,A], groupSize, tmp)(key)

  def sortBy[A : EncodeJson : DecodeJson, K : Ordering](xs: Iterable[A], groupSize: Int)(key: A => K)(implicit osSpecific: OSSpecific): Process[Task,A] =
    sortBy(emitAll(xs.toSeq): Process[Task,A], groupSize, Filesystem.mkTempDir())(key)

  def sortBy[A : EncodeJson : DecodeJson, K : Ordering](xs: Process[Task,A], chunkSize: Int, tmp: Path)(key: A => K): Process[Task,A] = {
    def grouped[I](n: Int): Process.Process1[I,Vector[I]] = {
      def go(m: Int, acc: Vector[I]): Process.Process1[I,Vector[I]] =
        if (m <= 0) emit(acc) ++ go(n, Vector())
        else await1[I].flatMap(i => go(m-1, acc :+ i))
      go(n, Vector.empty)
    }
    // sort chunks

    ((xs.map(_.some) ++ emitSeq(Vector.fill(chunkSize)(none))) |> grouped(chunkSize)).map(_.collect{ case Some(x) => x }).zipWithIndex.flatMap{ case (chunk, i) =>
      val out = tmp / i.toString

      (emitAll(chunk.sortBy(key)): Process[Task, A])
        .map{ x => x.asJson.toString  }
        .intersperse("\n")
        .pipe(text.utf8Encode)
        .to(io.fileChunkW(out.toString)).drain ++ emit(if (Filesystem.exists(out)) Vector(out) else Vector.empty)
    }.foldMonoid.lastOr(Vector.empty).flatMap{ paths =>
      // merge chunks
      implicit def kbOrdering[B] =
        new Ordering[(K,B)] { def compare(x: (K,B), y: (K,B)) = implicitly[Ordering[K]].reverse.compare(x._1, y._1) }

      def pop(iter: Iterator[A]) = if (iter.hasNext) Some((iter.next, iter)) else None

      lazy val inps = paths.map(p => new BufferedInputStream(new FileInputStream(p.toString)))
      lazy val sort = new Iterator[A] {
        private val q = PriorityQueue.empty[(K, (A, Iterator[A]))]

        private lazy val iters = {

          val is = inps.map{ i => scala.io.Source.fromInputStream(i).getLines
            .map(_.decodeEither[A].fold({ s => throw new Exception(s"failed to deserialize ($s)") }, identity))
          }
          is.foreach(i => pop(i).foreach{ case (a, rest) => q.enqueue((key(a), (a, rest))) })
          is
        }

        def hasNext = {
          iters
          q.nonEmpty
        }

        def next = {
          val (_, (a, i)) = q.dequeue()
          pop(i).foreach{ case (a, rest) => q.enqueue((key(a), (a, rest))) }
          a
        }
      }

      io.resource(Task.delay(sort))(_ =>
        Task.delay{ inps.foreach{ i => i.close }; Filesystem.delete(tmp, true) })(
        s => Task.delay{ if (s.hasNext) s.next else throw Process.End })
    }
  }
}
