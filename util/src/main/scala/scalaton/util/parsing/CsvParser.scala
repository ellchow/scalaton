package scalaton.util.parsing

import scala.util.parsing.combinator._

trait CsvParser extends RegexParsers {
  override val skipWhitespace = false
  // override val whiteSpace = """\s+""".r

  val DELIMITER: String = ","
  val QUOTE: String = "\""
  val CR: String = "\r"
  val LF: String = "\n"

  def CRLF = CR + LF
  def rowDelimiter = CR | LF | CRLF

  def wholeNumber: Parser[String] = """-?\d+""".r

  def floatingPointNumber: Parser[String] =
    """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r

  def unquoted: Parser[String] = s"[^${DELIMITER}${QUOTE}${CR}${LF}]*".r

  def quoted: Parser[String] = QUOTE ~> s"[^${QUOTE}]*".r <~ QUOTE

  def row: Parser[List[String]] = repsep(quoted | unquoted, DELIMITER)

  def header = row

  def rows: Parser[List[List[String]]] = repsep(row, rowDelimiter)

  def fromString: Parser[List[Option[Map[String,String]]]] = for{
    hdr <- header <~ rowDelimiter
    size = hdr.size
    rs <- rows
  } yield rs.map{ r =>
    if(r.size != size) None
    else Some(hdr zip r toMap)
  }

  def parse(s: String) = parseAll(fromString, s)
}
