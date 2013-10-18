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


