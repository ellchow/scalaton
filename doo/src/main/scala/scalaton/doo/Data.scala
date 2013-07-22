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

import scalaton.util.ProductFlatIterator

import com.nicta.scoobi.Scoobi._

import scalaz.{DList => _, _}
import Scalaz._

import org.slf4j.LoggerFactory

trait DataFunctions{

  def toDelimitedTextFileWithHeader[A <: Product : Manifest](dl: DList[A], header: Product,
                                                             path: String, sep: String = "\t",
                                                             noneString: String = "",
                                                             encode: String => String = _.replaceAll("\\s+"," "),
                                                             overwrite: Boolean = false,
                                                             headerPath: String ="header",
                                                             dataPath: String ="data")
  = {
    def unpackToString(prod: Product, hdr: Product): String = {
      require(prod.productArity == hdr.productArity, "record and header must have same arity: $prod%s    $hdr%s")

      prod.productIterator.zip(hdr.productIterator).map{
        case (None, h: Product) =>
          ProductFlatIterator(h).map(x => noneString).mkString(sep)

        case (Some(elem: Product), h: Product) =>
          unpackToString(elem, h)

        case (elem: Product, h: Product) =>
          unpackToString(elem, h)

        case (None, h: String) => noneString

        case (Some(elem), h: String) =>
          encode(elem.toString)

        case (elem: Any, h: String) => encode(elem.toString)
      }.mkString(sep)
    }

    val out = dl.map{x => unpackToString(x, header)}


    hdfs.write(path + "/" + headerPath, new ProductFlatIterator(header).mkString(sep) + "\n", overwrite)

    toTextFile(out, path + "/" + dataPath, overwrite)
  }

  def mapToDelimitedTextFileWithHeader(dl: DList[Map[String,String]], header: Seq[String],
                                       path: String, sep: String = "\t",
                                       defaultValue: Option[String] = None,
                                       encode: String => String = _.replaceAll("\\s+"," "),
                                       overwrite: Boolean = false,
                                       headerPath: String ="header",
                                       dataPath: String ="data")
  = {

    val out = dl.map{
      tuple => header.map{
        col => {
          encode(defaultValue match {
            case Some(x) => tuple.getOrElse(col, x)
            case None => tuple(col)
          })
        }
      }.mkString(sep)
    }

    hdfs.write(path + "/" + headerPath, header.mkString(sep) + "\n", overwrite = overwrite)

    toTextFile(out, path + "/" + dataPath, overwrite)
  }

  def fromDelimitedTextFileWithHeader(path: String, sep: String = "\t", headerPath: String ="header", dataPath: String ="data")
  : (Seq[String], DList[Map[String,String]]) = {
    val header = hdfs.getLines(path + "/" + headerPath).head.split(sep).toSeq

    val lines = fromTextFile(path + "/" + dataPath)

    val dl = lines.map{ line => header.zip(line.split(sep)).toMap }

    (header, dl)
  }

}

object data extends DataFunctions
