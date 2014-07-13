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

import java.io._

package object util {
  type Hashable32[A] = Hashable[A,Int]
  type Hashable64[A] = Hashable[A,Long]
  type Hashable128[A] = Hashable[A,(Long,Long)]

  implicit class InputStreamOps(in: InputStream){
    def gz = new java.util.zip.GZIPInputStream(in)
    def buffered(n: Int = 4096) = new BufferedInputStream(in, n)
  }

  implicit class OutputStreamOps(out: OutputStream){
    def gz = new java.util.zip.GZIPOutputStream(out)
    def buffered(n: Int = 4096) = new BufferedOutputStream(out, n)
    def printer = new PrintStream(out)
  }

  implicit class AnyRefToMap(x: AnyRef) {
    def toMap: Map[String,Any] = x.getClass.getDeclaredFields.foldLeft(Map.empty[String,Any]){ (a, f) =>
      f.setAccessible(true)
      a + (f.getName -> f.get(x))
    }
  }

  implicit class ThrowableOps(t: Throwable) {
      def stackTrace = {
          val out = new ByteArrayOutputStream
      val prs = new PrintStream(out)
      t.printStackTrace(prs)
      out.toString("UTF8")
    }
  }

  private val pidRegex = "([0-9]+)[@].*".r

  lazy val pid = {
    val s = java.lang.management.ManagementFactory.getRuntimeMXBean().getName()
    pidRegex.unapplySeq(s) match {
      case Some(i :: _) => i.toInt
      case _ => throw new RuntimeException(s"failed to parse pid")
    }
  }



  object Implicits {
    implicit val unixOsSpecific = new OSSpecific {
      val / = "/"
    }

    implicit def stringToJavaFile(p: String) = new File(p)
    implicit def stringToPath(p: String)(implicit osSpecific: OSSpecific) = Path(stringToJavaFile(p))(osSpecific)
    implicit def pathToJavaFile(p: Path) = p.file
  }
}
