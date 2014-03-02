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

package scalaton.util

import java.io._

object io {

  def file(f: File, subdirs: String*): File = if (subdirs.isEmpty) {
    f
  } else {
    file(new File(f, subdirs(0)), subdirs.drop(1): _*)
  }

  def file(path: String, subdirs: String*): File = file(new File(path), subdirs: _*)

  implicit class FileOps(val f: File) extends AnyVal {
    def resource = getClass.getClassLoader.getResourceAsStream(f.getPath)
    def asInput = new FileInputStream(f)
    def asOutput = new FileOutputStream(f)
  }

  implicit class InputStreamOps(val in: InputStream) extends AnyVal {
    def gz = new java.util.zip.GZIPInputStream(in)
    def buffered(n: Int = 4096) = new BufferedInputStream(in, n)
  }

  implicit class OutputStreamOps(val out: OutputStream) extends AnyVal {
    def gz = new java.util.zip.GZIPOutputStream(out)
    def buffered(n: Int = 4096) = new BufferedOutputStream(out, n)
  }

  implicit def fileToInputStream(f: File) = f.asInput

  implicit def fileToOutputStream(f: File) = f.asOutput

}
