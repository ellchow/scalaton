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
import scala.collection.mutable
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
    val ps = await(Task.delay{
      val res = \/.fromTryCatch{
        ((xs.map(_.some) ++ emitSeq(Vector.fill(chunkSize)(none))) |> process.grouped(chunkSize)).map(_.collect{ case Some(x) => x })
          .zipWithIndex
          .flatMap{ case (chunk, i) =>
            val out = tmp / i.toString

            (emitAll(chunk.sortBy(key)): Process[Task, A])
              .map{ x => x.asJson.toString  }
              .intersperse("\n")
              .pipe(text.utf8Encode)
              .to(io.fileChunkW(out.toString)).drain ++
            emit(if (Filesystem.exists(out)) Vector(out) else Vector.empty)
        }.foldMonoid.runLastOr(Vector.empty).run
      }
      res
    })(disj => disj.fold(e => Halt(e), paths => emit(paths)))

    ps.flatMap{ paths =>
      // merge chunks
      implicit def kbOrdering[B] =
        new Ordering[(K,B)] { def compare(x: (K,B), y: (K,B)) = implicitly[Ordering[K]].compare(x._1, y._1) }

      val inputs: Vector[Process[Task,(K, A)]] = paths.map{ p =>
        io.linesR(p.file.getAbsolutePath)
        .flatMap(_.decodeEither[A].fold(
          s => Halt(new Exception(s"failed to deserialize ($s)")),
          { a => emit((key(a), a)) }
        ))
      }

      def run(p: Process[Task,A]): Process[Task,A] = p match {
        case h@Halt(_) =>
          paths.foreach(p => Filesystem.delete(p))
          h
        case e@Emit(h, t) =>
          emitAll(h) ++ run(t)
        case Await(req,recv,fb,c) =>
          await(req)(recv.andThen(run _), run(fb), run(c))
      }

      run(Join.meldAll(inputs).map(_._2))
    }
  }
}
