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

package scalaton.util.parsing

import scala.util.parsing.combinator._

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

  def rows(hdr: List[String]): Parser[List[Map[String,String]]] = {
    val expectedSize = hdr.size

    repsep(row flatMap { r =>
      if(r.size != expectedSize) failure(s"invalid number of fields (got ${r.size}, expected $expectedSize)")
      else success(hdr zip r toMap)
    },
      rowDelimiter)
  }
  def fromString: Parser[List[Map[String,String]]] = for {
    hdr <- header <~ rowDelimiter
    rs <- rows(hdr)
  } yield rs

  def parse(s: String) = parseAll(fromString, s)
}

trait RequiredFields { this: CsvParser =>
  val required: Set[String]

  override def header = row flatMap { xs =>
    if((required -- xs).isEmpty) success(xs)
    else failure(s"missing fields: ${(required -- xs).mkString(",")}")
  }
}
