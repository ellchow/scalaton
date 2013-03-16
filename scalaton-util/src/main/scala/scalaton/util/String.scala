package scalaton.util

import scalaz._
import Scalaz._


trait StringModule{
  def splitByChar(s: String, splitChar: Char) =
    (s.foldLeft((new StringBuilder, Vector[String]()))
     { case ((sb,rest),ch) =>
       (ch === splitChar) ? (new StringBuilder, rest :+ sb.toString) | (sb += ch, rest)
     })._2


}

object str
extends StringModule
