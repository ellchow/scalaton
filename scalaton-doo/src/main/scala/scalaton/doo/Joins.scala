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

// import language.existentials

import scalaton.util._
import scalaton.util.hashing._
import scalaton.util.hashing128._

import scalaton.aggregate.hashed.hcollection._
import scalaton.aggregate.hashed.mutable.bloomfilter._
import com.googlecode.javaewah.{EWAHCompressedBitmap => CompressedBitSet}

import com.nicta.scoobi.Scoobi._


import scalaz.{DList => _, _}
import Scalaz._


trait JoinFunctions{
  def bloomJoin[A : Manifest : WireFormat : Grouping, BL : Manifest : WireFormat, BR : Manifest : WireFormat](
    left: DList[(A,BL)], right: DList[(A,BR)], expectedNumKeys: Int)(
    implicit hashableA: Hashable[A, Bits128]) = {

    trait SBF
    implicit val sbfinst = sbf.sparse[A, Bits128, SBF](sbf.optimalParameters(expectedNumKeys, 0.1))
    implicit val cbsWF = AnythingFmt[CompressedBitSet @@ SBF]

    val leftKeys = helpers.accumulate[A, CompressedBitSet @@ SBF](left map ( _._1 ))((acc, x) => insert(acc, x))

    val rightFiltered = leftKeys.join(right).mapFlatten{ case (ks, (a, br)) =>
                                                         contains(ks, a) ? (a, br).some | none[(A,BR)] }

    left join rightFiltered
  }
}

object joins extends JoinFunctions
