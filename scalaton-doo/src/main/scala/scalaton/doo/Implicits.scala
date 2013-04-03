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

import scalaton.util._
import scalaton.util.hashing._
import scalaton.util.hashing32._

import com.nicta.scoobi.Scoobi._

import scalaz.{DList => _, _}
import Scalaz._

trait EnrichedDList[A]{
  val dl: DList[A]
}

trait ImplicitConversions{

  private[doo] case class DList1WithHashable32[A : Manifest : WireFormat](val dl: DList[A])(implicit hashable: Hashable[A,Bits32]){
    def sample(rate: Double, seed: Int = 0) = sampling.sample(dl, rate, seed)
  }

  private[doo] case class DList2WithHashable32A[A : Manifest : WireFormat, B : Manifest : WireFormat](val dl: DList[(A,B)])(implicit hashable: Hashable[A,Bits32]){
    def sampleBy(rate: Double, seed: Int = 0) = sampling.sampleBy(dl, rate, seed)
  }

  private[doo] case class DList2WithHashable32GroupingA[A : Manifest : WireFormat : Grouping, B : Manifest : WireFormat](val dl: DList[(A,B)])(implicit hashable: Hashable[A,Bits32]){
    def bloomJoin[BR : Manifest : WireFormat](right: DList[(A,BR)], expectedNumKeys: Int) = joins.bloomJoin(dl, right, expectedNumKeys)

    def skewedJoin[BR : Manifest : WireFormat](right: DList[(A,BR)], sampleRate: Double, maxPerReducer: Int) = joins.skewedJoin(dl, right, sampleRate, maxPerReducer)
  }

  private[doo] case class DListRich[A : Manifest : WireFormat](val dl: DList[A]){
    def partitionAtRandom(n: Int, seed: Int = 0) = sampling.partitionAtRandom(dl, n, seed)

    def limit(n: Int = 0) = sampling.limit(dl, n)
  }


  implicit def enrichDList[A : Manifest : WireFormat](x: DList[A]) =
    DListRich(x)

  implicit def enrichDListWithHashable32[A : Manifest : WireFormat](x: DList[A])(implicit hashable: Hashable[A,Bits32]) =
    DList1WithHashable32(x)

  implicit def enrichDList2WithHashable32A[A : Manifest : WireFormat, B : Manifest : WireFormat](x: DList[(A,B)])(implicit hashable: Hashable[A,Bits32]) =
    DList2WithHashable32A(x)

  implicit def enrichDList2WithHashable32GroupingA[A : Manifest : WireFormat : Grouping, B : Manifest : WireFormat](x: DList[(A,B)])(implicit hashable: Hashable[A,Bits32]) =
    DList2WithHashable32GroupingA(x)


}

object implicits extends ImplicitConversions
