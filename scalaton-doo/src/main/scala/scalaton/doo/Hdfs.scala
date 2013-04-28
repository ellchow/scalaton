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

import scalaton.zed.io._

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


  def delete(path: String, recursive: Boolean, conf: HConf = new HConf): Boolean = {
    val fs = connect(conf)
    val z = fs.delete(new HPath(path), recursive)
    fs.close
    z
  }


  def exists(path: String, conf: HConf = new HConf) = {
    val fs = connect(conf)
    val z = fs.exists(new HPath(path))
    fs.close
    z
  }

  def isComplete(path: String, conf: HConf = new HConf) = {
    val fs = connect(conf)
    val z = fs.exists(new HPath(path, "_SUCCESS"))
    fs.close
    z
  }

  def write(path: String, s: String, overwrite: Boolean = false, conf: HConf = new HConf): Unit = {
    val fs = connect(conf)
    val p = out(fs, path, overwrite)

    write(p, s)

    fs.close
    p.close
  }

  def getLines(path: String, conf: HConf = new HConf): Iterable[String] = {
    val fs = connect(conf)
    val p = in(fs, path)

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
    }
  }
}

object hdfs extends HdfsFunctions
