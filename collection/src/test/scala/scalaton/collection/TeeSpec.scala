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

package scalaton.collection

import org.scalacheck._
import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._
import scala.util.{ Try, Success, Failure }
import scalaton.util.paths._, Implicits._

class TeeSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  import Tee._
  import argonaut._, Argonaut._

  behavior of "tee"

  it should "partition elements by left and right" in {
    forAll {
      (xs: Set[Either[Int,Int]]) => {
        val out = Filesystem.mkTemp()
        try {
          val rights = xs.iterator.tee(new java.io.FileOutputStream(out.file)).map(x => Right(x)).toSet
          val lefts = scala.io.Source.fromFile(out.file).getLines.flatMap(_.decodeOption[Int].map(x => Left(x)))

          xs should be(rights ++ lefts)
        } finally {
          Filesystem.delete(out)
        }
      }
    }
  }

}
