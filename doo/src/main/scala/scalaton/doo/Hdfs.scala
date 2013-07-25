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

package scalaton.doo

import util.control.Exception._

import scalaton.util.io._

import org.apache.hadoop.fs.{Path => HPath, _}
import org.apache.hadoop.conf.{Configuration => HConf}

import scalaz._
import Scalaz._
import Validation.fromTryCatch


trait HdfsFunctions{

  def connect(conf: HConf = new HConf): FileSystem = FileSystem.get(conf)

  def out(fs: FileSystem, path: String, overwrite: Boolean = false): FSDataOutputStream =
    fs.create(new HPath(path), overwrite)

  def in(fs: FileSystem, path: String): FSDataInputStream =
    fs.open(new HPath(path))

  def write(out: FSDataOutputStream, s: String): Unit =
    out.write(s.getBytes("UTF8"))

  def exec[A](f: FileSystem => A, conf: HConf = new HConf) = {
    val fs = connect(conf)
    val a = f(fs)
    fs.close
    a
  }

  def delete(path: String, recursive: Boolean, conf: HConf = new HConf): Boolean =
    exec(fs => fs.delete(new HPath(path), recursive), conf)


  def fileStatus(path: String, conf: HConf = new HConf) =
    exec(fs => fs.getFileStatus(new HPath(path)), conf)

  def isDirectory(path: String, conf: HConf = new HConf) =
    fileStatus(path, conf).isDir

  def exists(path: String, conf: HConf = new HConf) =
    exec(fs => fs.exists(new HPath(path)), conf)

  def isComplete(path: String, conf: HConf = new HConf) =
    exec(fs => fs.exists(new HPath(path, "_SUCCESS")), conf)

  def write(path: String, s: String, overwrite: Boolean = false, conf: HConf = new HConf): Unit =
    exec(fs => {
      val p = out(fs, path, overwrite)
      write(p, s)
      p.close
    })

  def getLines(path: String, conf: HConf = new HConf): Iterable[String] =
    exec(fs => {
      val p = in(fs, path)

      val r = reader.inputStream(p)

      new Iterable[String]{
        def iterator = new Iterator[String]{
          private var nextLine = r readLine

          def hasNext = {
            val test = nextLine != null

            if(!test)
              p.close

            test
          }

          def next = {
            val out = nextLine

            nextLine = r readLine

            out
          }
        }
      }
    })
}

object hdfs extends HdfsFunctions
