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

package scalaton.doo

import com.nicta.scoobi.Scoobi._

import scalaz.{DList => _, _}
import Scalaz._

trait GroupingFunctions{
  def secondarySort[K : Grouping : Order, S: Order] = new Grouping[(K, S)] {
    override def partition(key: (K, S), howManyReducers: Int) =
      implicitly[Grouping[K]] partition (key._1, howManyReducers)

    override def sortCompare(a: (K, S), b: (K, S)) = a ?|? b

    override def groupCompare(a: (K, S), b: (K, S)) = a._1 ?|? b._1

  }
}

object grouping extends GroupingFunctions
