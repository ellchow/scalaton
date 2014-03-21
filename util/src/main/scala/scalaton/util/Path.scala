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

trait path {
  implicit def stringToJavaFile(p: String) = new File(p)
  implicit def stringToPath(p: String) = Path(stringToJavaFile(p))

  case class Path private[util] (f: File) {
    def /(s: String) = path(f, s)

    def parent: Option[Path] = Option(f.getParentFile).map(pf => Path(pf))

    def name = f.getName

    def absolute = path(f.getAbsolutePath)

    def unlinked = path(f.getCanonicalPath)

    override def toString = f.getCanonicalPath
  }

  def path(f0: File, cs: String*) =
    Path(cs.foldLeft(f0)((f, c) => new File(f, c)))

  val / = path("/")

  def home = path(System.getProperty("user.home"))
  def ~ = home
  def pwd = path(System.getProperty("user.dir"))

  object fs {
    def isRoot(p: Path) = p == /

    def exists(p: Path) = p.f.exists

    def touch(p: Path) = FileUtils.touch(p.f)

    def mkdir(p: Path, parents: Boolean = false) =
      if (parents) p.f.mkdirs else p.f.mkdir

    def write(p: Path, s: String, append: Boolean = true, encoding: java.nio.charset.Charset = null) =
      FileUtils.writeStringToFile(p.f, s, encoding, append)

    def isDirectory(p: Path) = p.f.isDirectory

    def dirSize(p: Path) = BigInt(FileUtils.sizeOfDirectoryAsBigInteger(p.f))

    def size(p: Path) = BigInt(FileUtils.sizeOfAsBigInteger(p.f))

    def listDir(root: Path, recurse: Boolean = false, unlink: Boolean = false): Stream[Path] = {
      def lst(p: Path) =
        Option((if (unlink) p.unlinked else p).f.listFiles) match {
          case Some(pp) => pp.toList.map(ff => Path(ff)).partition(x => !isDirectory(x))
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
  }
}

object path extends path
