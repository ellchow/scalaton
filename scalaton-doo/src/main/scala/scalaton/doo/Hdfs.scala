package scalaton.doo

import util.control.Exception._

import scalaton.zed.io._

import org.apache.hadoop.fs.{Path => HPath, _}
import org.apache.hadoop.conf.{Configuration => HConf}

import scalaz._
import Scalaz._
import Validation.fromTryCatch


trait hdfs{

  def connect(conf: HConf = new HConf): FileSystem = FileSystem.get(conf)

  def out(fs: FileSystem, path: String, overwrite: Boolean = false): FSDataOutputStream =
    fs.create(new HPath(path), overwrite)

  def in(fs: FileSystem, path: String): FSDataInputStream =
    fs.open(new HPath(path))

  def write(out: FSDataOutputStream, s: String): Unit =
    out.write(s.getBytes("UTF8"))

  def write(path: String, s: String, overwrite: Boolean = false, conf: HConf = new HConf): Validation[Throwable,Unit] =
    fromTryCatch{
      val fs = connect(conf)
      val p = out(fs, path, overwrite)

      (fs, p)
    }.fold(
      _.failure[Unit],
      { case (fs, p) =>
        val w = fromTryCatch(write(p, s))
        fs.close
        p.close
        w
      }
    )

  def getLines(path: String, conf: HConf = new HConf): Validation[Throwable, Iterable[String]] = fromTryCatch{
    val fs = connect(conf)
    val p = in(fs, path)

    (fs, p)
  }.fold(
    _.failure[Iterable[String]],
    { case (fs, p) =>
      fromTryCatch{
        val r = reader.inputStream(p)

        new Iterable[String]{
        def iterator = new Iterator[String]{
          private var nextLine = r readLine

          def hasNext = {
            val test = nextLine != null
            if(!test){
              p.close
              fs.close
            }
            test
          }

          def next = {
            val out = nextLine

            nextLine = r readLine

            out
          }
        }
      }}
    }
  )
}
