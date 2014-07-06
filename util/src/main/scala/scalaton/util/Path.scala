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

package scalaton.util

import java.io._
import scalaz._, Scalaz._
import org.apache.commons.io.FileUtils

trait OSSpecific {
  val / : String

  def path(f0: File, cs: String*) = Path(cs.foldLeft(f0)((f, c) => new File(f, c)))(this)
}


case class Path private[util] (val file: File)(implicit osSpecific: OSSpecific) {
  def /(s: String) = osSpecific.path(file, s)

  def parent: Option[Path] = Option(file.getParentFile).map(pf => Path(pf))

  def name = file.getName

  def absolute = Path(new File(file.getAbsolutePath))

  def unlinked = Path(new File(file.getCanonicalPath))

  override def toString = file.getCanonicalPath
}


object paths {
  def path(f0: File, cs: String*)(implicit osSpecific: OSSpecific) = osSpecific.path(f0, cs : _*)
  def root(implicit osSpecific: OSSpecific) = path(new File(osSpecific./))
  def home(implicit osSpecific: OSSpecific) = path(new File(System.getProperty("user.home")))
  def pwd(implicit osSpecific: OSSpecific) = path(new File(System.getProperty("user.dir")))

  object Filesystem {
    def isRoot(p: Path)(implicit osSpecific: OSSpecific) = p == path(new File(osSpecific./))

    def exists(p: Path) = p.file.exists

    def touch(p: Path) = FileUtils.touch(p.file)

    def delete(p: Path, recursive: Boolean = false) =
      if (recursive) FileUtils.deleteDirectory(p.file) else p.file.delete

    def mkdir(p: Path, parents: Boolean = false) =
      if (parents) p.file.mkdirs else p.file.mkdir

    def write(p: Path, s: String, append: Boolean = true, encoding: java.nio.charset.Charset = null) =
      FileUtils.writeStringToFile(p.file, s, encoding, append)

    def isDirectory(p: Path) = p.file.isDirectory

    def dirSize(p: Path) = BigInt(FileUtils.sizeOfDirectoryAsBigInteger(p.file))

    def size(p: Path) = BigInt(FileUtils.sizeOfAsBigInteger(p.file))

    def resource(p: Path) = getClass.getClassLoader.getResourceAsStream(p.file.getPath)

    def inputStream(p: Path) = new FileInputStream(p.file)

    def outputStream(p: Path) = new FileOutputStream(p.file)

    def listDir(root: Path, recurse: Boolean = false, unlink: Boolean = false)(implicit osSpecific: OSSpecific): Stream[Path] = {
      def lst(p: Path) =
        Option((if (unlink) p.unlinked else p).file.listFiles) match {
          case Some(pp) => pp.toList.map(ff => path(ff)).partition(x => !isDirectory(x))
          case None => throw new IllegalArgumentException(s"$root does not exist")
        }

      def loop(paths: List[Path], dirs: List[Path]): Stream[Path] =
        paths match {
          case p :: ps => p #:: loop(ps, dirs)
          case _ => dirs match {
            case d :: ds =>
              val (nextFiles, nextDirs) = lst(d)
              d #:: (if (recurse) loop(nextFiles, ds ++ nextDirs) else Stream.empty )
            case _ => Stream.empty
          }
        }

      val start = lst(root)
      loop(start._1, start._2)
    }

    def mkTempDir(baseFile: File = new File(System.getProperty("java.io.tmpdir")), attempts: Int = 1000)(implicit osSpecific: OSSpecific): Path = {
      val base = path(baseFile)
      val timestamp = System.currentTimeMillis + "-"

      var i = 0
      var tmp = None: Option[Path]
      while(tmp.isEmpty && i < attempts) {
        i += 1
        val t = base / (timestamp + "-" + i)

        if (mkdir(t)) tmp = Some(t)
      }

      if(tmp.nonEmpty)
        tmp.get
      else
        throw new Exception(s"failed to create temp dir in $base after $attempts attempts")
    }

    def mkTemp(baseFile: File = new File(System.getProperty("java.io.tmpdir")), attempts: Int = 1000)(implicit osSpecific: OSSpecific) = {
      val p = mkTempDir(baseFile, attempts) / "file"
      touch(p)
      p
    }
  }
}
