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
import scalaz._, Scalaz._

object Tee {
  /* writes left elements in the iterator to an output stream and emits the right values */
  implicit class Tee[T,O](iter: Iterator[Either[T,O]]) {
    def tee(out: OutputStream, delim: Array[Byte])(ser: T => Array[Byte]): Iterator[O] =
      new Iterator[Either[T,O]] {
        def hasNext = {
          if (!iter.hasNext) out.close
          iter.hasNext
        }

        def next = {
          val x = iter.next()

          x match {
            case Left(t) =>
              out.write(ser(t))
              out.write(delim)

            case _ => // do nothing
          }

          x
        }
      }.collect{ case Right(o) => o }

    def tee(out: OutputStream, delim: String = "\n")(ser: T => String): Iterator[O] =
      tee(out, delim.getBytes)(ser.andThen(_.getBytes))

    def tee(out: OutputStream)(implicit t: EncodeJson[T]): Iterator[O] =
      tee(out, "\n")(_.asJson.toString)

  }

  /* conversion to tee everything to file while mirroring to output */
  implicit def toTeeId[A](iter: Iterator[A]) = new Tee[A,A](iter.flatMap(a => Seq(Right(a), Left(a))))

  /* conversion of \/ to Either */
  implicit def teeScalazEither[T,O](iter: Iterator[\/[T,O]]) = new Tee[T,O](iter.map(_.toEither))

}
