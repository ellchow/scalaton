package scalaton.util

import scalaz._
import Scalaz._


trait StringModule{
  def splitByChar(s: String, splitChar: Char): Vector[String] = {
    val (last, rest) = (s.foldLeft((new StringBuilder, Vector[String]()))
         { case ((sb,rest),ch) =>
           (ch === splitChar) ? (new StringBuilder, rest :+ sb.toString) | (sb += ch, rest)
         })

    rest :+ last.toString
  }


}

object str
extends StringModule
