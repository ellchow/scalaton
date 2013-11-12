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

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._

import org.scalacheck._

class HashableSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  behavior of "Hashable"

  import scalaton.util.hashing128._

  it should "hash string 'hello'" in {
    val hc = hash("hello")

    hc should be((-4758432102323878981L,1262627326183304356L))
  }

  it should "hash multiple times" in {
    forAll{
      (s: Long, nn: Int) => whenever(nn > 0){
        val n = nn % 1000
        multiHash("hello", s).take(n).size should be(n)
      }
    }
  }

  it should "hash to mostly distinct values" in {
    forAll{
      (ss: Set[String]) => {
        ss.map(s => hash(s)).size should be >= (0.99 * ss.size).toInt
      }
    }
  }

}
