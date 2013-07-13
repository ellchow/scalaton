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

package scalaton.codegen

import com.github.nscala_time.time.Imports._

import scalaz._
import Scalaz._


/**
 * Code generate HashableTupleInstances
 * TODO: incorporate into sbt build
 **/
object HashableTupleGen{

  def gen(bits: Int, datatype: String, init: String) = {

    val funcString = (1 to 22) map { sz =>
      val ns = 1 to sz
      val typeParams = ns map { n => f"A$n"  } mkString ","
      val implicitParams = ns map { n => f"h$n%d: Hashable[A$n%d,$datatype%s]" } mkString ", "
      val combineStmts = ns map { n =>
        f"current = combine$bits%dHashes(current, h$n%d.digest(a._$n%d, seed))"
      } mkString "\n      "

      f"""  implicit def tuple$sz%dHashable$bits%d[$typeParams](implicit $implicitParams) = new Hashable[Tuple$sz%d[$typeParams],$datatype%s]{
      def digest(a: Tuple$sz%d[$typeParams], seed: Long): $datatype%s @@ HashCode = {
      var current: $datatype%s = $init%s
      $combineStmts
      HashCode(current)
      }\n  }"""
    } mkString "\n"

    f"trait HashableTuple$bits%sInstances extends HashFuncs{\n$funcString%s\n}"
  }

  def main(args: Array[String]){
    import java.io.File
    import org.apache.commons.io.FileUtils

    val genDate = (new LocalDateTime) toString

    val comment = f"""
/**
  * Implicits for Hashable Tuples
  * DO NOT EDIT - code generated on $genDate%s. see scalaton/codegen/HashableTupleGen.scala
  **/
  """

    val traits = Seq(gen(32, "Int", "1"),
                     gen(64, "Long", "1L"),
                     gen(128, "(Long, Long)", "(1L, 1L)")).mkString("\n")

    FileUtils.writeStringToFile(new File("scalaton-util/src/main/scala/scalaton/util/HashableTuple.scala"),
                                f"""/*
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

package scalaton.util\n\nimport scalaz._\nimport Scalaz._\n$comment%s\n$traits%s"""
                              )
  }
}

