package scalaton.util.parsing

import scala.util.parsing.combinator._
import scalaz._, Scalaz._

trait CsvParser extends RegexParsers {
  override val skipWhitespace = false

  val DELIMITER: String = ","
  val QUOTE: String = "\""
  val CR: String = "\r"
  val LF: String = "\n"

  def CRLF = CR + LF

  def rowDelimiter: Parser[String] = CR | LF | CRLF

  def wholeNumber: Parser[String] = """-?\d+""".r

  def floatingPointNumber: Parser[String] =
    """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r

  def unquoted: Parser[String] = s"[^${DELIMITER}${QUOTE}${CR}${LF}]*".r

  def quoted: Parser[String] = QUOTE ~> s"[^${QUOTE}]*".r <~ QUOTE

  def field: Parser[String] = quoted | unquoted

  def row: Parser[List[String]] = repsep(field, DELIMITER)

  def header = row

  def rows: Parser[List[List[String]]] = repsep(row, rowDelimiter)

  def fromString: Parser[List[ValidationNel[String,Map[String,String]]]] = for{
    hdr <- header <~ rowDelimiter
    size = hdr.size
    rs <- rows
  } yield rs.zipWithIndex.map{ case (r, i) =>
      if(r.size != size) s"invalid number of fields at line $i (got ${r.size}, expected $size)".failureNel
      else (hdr zip r toMap).successNel
  }

  def parse(s: String) = parseAll(fromString, s)
}

trait RequiredFields { this: CsvParser =>
  val required: Set[String]

  override def header = row.flatMap{ xs =>
    if((required -- xs).isEmpty) success(xs)
    else failure(s"missing fields: ${(required -- xs).mkString(",")}")
  }
}
