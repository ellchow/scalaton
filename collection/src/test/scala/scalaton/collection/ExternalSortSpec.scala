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

class ExternalSortSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  behavior of "ExternalSort"

  it should "be the same as sorting in memory" in {
    forAll {
      (xs: List[Int]) => {
        val tmp = scalaton.util.mkTempDir()
        val sorted = xs.sorted

        try {
          val externalsorted = ExternalSort.sortBy(xs.iterator, 10, tmp)(identity).flatMap(_.convert(_.toList))
          externalsorted should be(scala.util.Success(sorted))
        } finally {
          org.apache.commons.io.FileUtils.deleteDirectory(tmp.file)
        }
      }
    }
  }

}
