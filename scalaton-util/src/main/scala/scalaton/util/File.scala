package scalaton.util

import scalaz._
import Scalaz._
import Validation.fromTryCatch

import java.io.{BufferedReader,InputStreamReader,Reader,FileReader,BufferedWriter,OutputStreamWriter,Writer,FileWriter,File}
import org.apache.commons.io.FileUtils
import au.com.bytecode.opencsv._


object file
extends FileImplicits{

  type Path = String @@ FilePath

  def path(x: String): Path = Tag(x)

  def root = path("/")

  object Csv{
    def apply(r: CSVReader) = {
      val csv = new RichCsvReader(r)

      ((h: String) => (row: Vector[String]) => row(csv.header(h)), csv toStream)
    }

    def apply(reader: Reader, separator: Char = '\t', hasHeader: Boolean = true,
              quotechar: Char = '"', escape: Char = '\'', line: Int = 0,
              strictQuotes: Boolean = false, ignoreLeadingWhiteSpace: Boolean = true) = {
      val csv = new RichCsvReader(new CSVReader(reader, separator, quotechar, escape,
                                                line, strictQuotes,ignoreLeadingWhiteSpace),
                                  hasHeader)


      ((h: String) => (row: Vector[String]) => row(csv.header(h)), csv toStream)
    }


    def save(table: Iterable[Seq[Any]], writer: Writer, separator: Char = '\t', quotechar: Char = '\u0000', escapechar: Char = '\u0000', lineEnd: String = "\n"){
      val w = new CSVWriter(writer, separator, quotechar, escapechar, lineEnd)

      table.foreach(row => w.writeNext(Array[String](row.map(_.toString) : _*)))

      w.close
    }

  }

  class RichCsvReader(val csvReader: CSVReader, val hasHeader: Boolean = true) extends Iterable[Vector[String]]{

    val header: Map[String,Int] = if(hasHeader) csvReader.readNext.zipWithIndex.toMap else Map.empty

    private var nextRow: Array[String] = csvReader.readNext

    def iterator = new Iterator[Vector[String]]{

      def hasNext: Boolean = {
        val result = nextRow != null
        result
      }

      def next = {
        val ret = nextRow
        nextRow = csvReader.readNext

        Vector(ret: _*)
      }

    }
  }

}

sealed trait FilePath

trait FileImplicits{
  implicit def stringToReader(s: String @@ FilePath) =
      if(s.length == 0)
        new BufferedReader(new InputStreamReader(System.in))
      else
        new BufferedReader(new FileReader(s))

  implicit def optionStringToReader(s: Option[String @@ FilePath]) =
    stringToReader(Tag(s.getOrElse("")))

  implicit def stringToWriter(s: String @@ FilePath) =
      if(s.length == 0)
        new BufferedWriter(new OutputStreamWriter(System.out))
      else
        new BufferedWriter(new FileWriter(s))

  implicit def optionStringToWriter(s: Option[String @@ FilePath]) =
    stringToWriter(Tag(s.getOrElse("")))

  implicit def toPathOps(s: String @@ FilePath) = new FilePathOps{ val run = s }

}

trait FilePathOps {
  val run: String @@ FilePath

  def / (s: String): String @@ FilePath = Tag(new File(run,s) getPath)

  def parent : String @@ FilePath = Tag(new File(run) getParent)

  def isDir = new File(run) isDirectory

  def touch = FileUtils touch (new File(run))

  def exists = new File(run) exists

  def mkdir = new File(run) mkdir

  def mkdirp = new File(run) mkdirs

}

