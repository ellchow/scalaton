/*
 Copyright 2014 Elliot Chow

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

package scalaton

package object util {
  import java.io._

  def mkTempDir(base: scalaton.util.path.Path = scalaton.util.path.path(new File(System.getProperty("java.io.tmpdir"))), attempts: Int = 1000): scalaton.util.path.Path = {
    val timestamp = System.currentTimeMillis + "-"

    var i = 0
    var tmp = None: Option[scalaton.util.path.Path]
    while(tmp.isEmpty && i < attempts) {
      i += 1
      val t = base / (timestamp + "-" + i)

      if (scalaton.util.path.fs.mkdir(t)) tmp = Some(t)
    }

    if(tmp.nonEmpty)
      tmp.get
    else
      throw new Exception(s"failed to create temp dir in $base after $attempts attempts")
  }

  def mkTemp(base: scalaton.util.path.Path = scalaton.util.path.path(new File(System.getProperty("java.io.tmpdir"))), attempts: Int = 1000) = {
    val p = mkTempDir(base, attempts) / "file"
    scalaton.util.path.fs.touch(p)
    p
  }

  implicit class AnyRefToMap(x: AnyRef) {
    def toMap: Map[String,Any] = x.getClass.getDeclaredFields.foldLeft(Map.empty[String,Any]){ (a, f) =>
      f.setAccessible(true)
      a + (f.getName -> f.get(x))
    }
  }
}
