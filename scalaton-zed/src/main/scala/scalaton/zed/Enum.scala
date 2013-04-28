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

trait EnumFunctions{

  def fromEnumeration[A <: Enumeration](enumeration: A) = new Enum[enumeration.Value]{
    private val xs = enumeration.values.toSeq
    private val n = xs.size
    private lazy val lookup = xs.zipWithIndex.toMap

    override val max = (n > 0) ? xs(n - 1).some | none

    override val min = (n > 0) ? xs(0).some | none

    def succ(a: enumeration.Value) =
      xs((lookup(a) + 1) % n)

    def pred(a: enumeration.Value) =
      xs(math.abs((lookup(a) - 1) % n))

    def order(a1: enumeration.Value, a2: enumeration.Value) = lookup(a1) ?|? lookup(a2)
  }

}

object enum
extends DateEnumInstances
with EnumFunctions
