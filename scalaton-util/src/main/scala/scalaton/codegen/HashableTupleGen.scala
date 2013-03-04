package scalaton.codegen

import scalaz._
import Scalaz._


/**
 * Code generate HashableTupleInstances
 * TODO: incorporate into sbt build
 **/
object HashableTupleGen{

  def gen128 = {
    val funcString = (1 to 22) map { sz =>
      val ns = 1 to sz
      val typeParams = ns map { n => f"A$n"  } mkString ","
      val implicitParams = ns map { n => f"h$n%d: Hashable[A$n%d,(Long,Long)]" } mkString ", "
      val combineStmts = ns map { n =>
        f"current = combine128Hashes(current, h$n%d.digest(a._$n%d, seed))"
      } mkString "\n      "

      f"""  implicit def tuple$sz%dHashable128[$typeParams](implicit $implicitParams) = new Hashable[Tuple$sz%d[$typeParams],(Long,Long)]{
    def digest(a: Tuple$sz%d[$typeParams], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      $combineStmts
      HashCode(current)
    }\n  }"""
    } mkString "\n"

    val comment = """/**
 * Implicits for Hashable Tuples
 * DO NOT EDIT - code generated. see scalaton/codegen/HashableTupleGen.scala
 **/
"""

    f"package scalaton.util\n\nimport scalaz._\nimport Scalaz._\n$comment%s\ntrait HashableTupleInstances extends HashFuncs{\n$funcString%s\n}"
  }

  def main(args: Array[String]){
    import java.io.File
    import org.apache.commons.io.FileUtils

    FileUtils.writeStringToFile(new File("src/main/scala/util/HashableTuple.scala"),
                                gen128)

  }
}

