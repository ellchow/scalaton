package scalaton.collection

import scalaz._, Scalaz._
import argonaut._, Argonaut._
import java.io._

object Tee {

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

  implicit def toTeeId[A](iter: Iterator[A]) = new Tee[A,A](iter.flatMap{ a => Seq(Right(a), Left(a)) })

}
