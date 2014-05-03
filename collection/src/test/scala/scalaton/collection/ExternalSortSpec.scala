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

import scalaton.util.paths._, Implicits._
import org.scalacheck._
import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._
import scalaz._, Scalaz._
import scalaz.stream._
import scalaz.concurrent._


class ExternalSortSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  behavior of "ExternalSort"

  it should "be the same as sorting in memory" in {
    forAll {
      (xs: List[Int]) => {
        val tmp = Filesystem.mkTempDir()
        val sorted = xs.sorted
        val externalsorted = ExternalSort.sort(xs, 10, tmp).runLog.run

        externalsorted should be(sorted)
        Filesystem.exists(tmp) should be(false)
      }
    }
  }

}
