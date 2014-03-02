package scalaton

import scalaz.concurrent._
import scalaz.stream._
import java.io._
import argonaut._, Argonaut._

package object stream {
  implicit class ProcessCompanionOps(val p: Process.type) extends AnyVal {
    def from[A](iterator: Iterator[A]): Process[Task, A] =
      Process.unfold(iterator)(i => if(i.hasNext) Some((i.next, i)) else None)

    def from[A](iterable: Iterable[A]): Process[Task, A] = from(iterable.iterator)
  }

  implicit class ProcessIOCompanionOps(val p: io.type) extends AnyVal {
    def jsonR[A : DecodeJson](in: InputStream) =
      p.linesR(in).map(_.decodeEither[A])

    def linesW(os: =>OutputStream, bufferSize: Int = 4096, gz: Boolean = false) = {
      val eol = "\n".getBytes
      p.resource(Task.delay(os))(os => Task.delay(os.close))(
        os => Task.now((line: String) => Task.delay{ os.write(line.getBytes) ; os.write(eol) }))
    }

    def jsonW[A : EncodeJson](os: =>OutputStream, bufferSize: Int = 4096, gz: Boolean = false) = {
      val eol = "\n".getBytes
      p.resource(Task.delay(os))(os => Task.delay(os.close))(
        os => Task.now((a: A) => Task.delay{ os.write(a.asJson.toString.getBytes) ; os.write(eol) }))
    }
  }

  implicit class ProcessOps[+O](val p: Process[Task,O]) extends AnyVal {
    def zipWithIndex: Process[Task,(O, Int)] = p.zip(Process.constant(0).scan(0)((n, zero) => n + 1))
  }
}

// val x = Process.from(1 to 20).map(_ => util.Random.nextInt.toString).chunk(4).map(xs => Process.from(xs.sorted)).zipWithIndex.map{ case (p, i) => p.to(io.linesW(file(s"tmp-$i.txt"))).run.run; s"tmp-$i.txt" }.run.run