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

package scalaton.zed

import com.github.nscala_time.time.Imports._

import scalaz._
import Scalaz._

trait DateEnumInstances{
  implicit val localDateInstance = new Enum[LocalDate] {
    def succ(a: LocalDate): LocalDate = a + 1.day

    def pred(a: LocalDate): LocalDate = a - 1.day

    def order(a: LocalDate, b: LocalDate) = a.compareTo(b) match {
      case -1 => Ordering.LT
      case 0 => Ordering.EQ
      case 1 => Ordering.GT
    }
  }
}

object enum
extends DateEnumInstances
