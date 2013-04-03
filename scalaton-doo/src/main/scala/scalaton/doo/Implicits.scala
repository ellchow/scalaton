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

  private[doo] case class DListWithSample[A : Manifest : WireFormat](val dl: DList[A])(implicit hashable: Hashable[A,Bits32]){
    def sample(rate: Double, seed: Int = 0) = sampling.sample(dl, rate, seed)
  }

  private[doo] case class DListWithSampleBy[A : Manifest : WireFormat, B : Manifest : WireFormat](dl: DList[(A,B)])(implicit hashable: Hashable[A,Bits32]){
    def sampleBy(rate: Double, seed: Int = 0) = sampling.sampleBy(dl, rate, seed)
  }

  private[doo] case class DListWithLimit[A : Manifest : WireFormat](val dl: DList[A]){
    def limit(n: Int = 0) = sampling.limit(dl, n)
  }

  private[doo] case class DListWithPartitionAtRandom[A : Manifest : WireFormat](val dl: DList[A]){
    def partitionAtRandom(n: Int, seed: Int = 0) = sampling.partitionAtRandom(dl, n, seed)
  }

  private[doo] case class DListWithBloomJoin[A : Manifest : WireFormat : Grouping, BL : Manifest : WireFormat](val dl: DList[(A, BL)])(implicit hashable: Hashable[A,Bits32]){
    def bloomJoin[BR : Manifest : WireFormat](right: DList[(A,BR)], expectedNumKeys: Int) = joins.bloomJoin(dl, right, expectedNumKeys)
  }

  private[doo] case class DListWithSkewedJoin[A : Manifest : WireFormat : Grouping, BL : Manifest : WireFormat](val dl: DList[(A, BL)])(implicit hashable: Hashable[A,Bits32]){
    def skewedJoin[BR : Manifest : WireFormat](right: DList[(A,BR)], sampleRate: Double, maxPerReducer: Int) = joins.skewedJoin(dl, right, sampleRate, maxPerReducer)
  }


  implicit def enrichDListWithSample[A : Manifest : WireFormat](x: DList[A])(implicit hashable: Hashable[A,Bits32]) =
    DListWithSample(x)

  implicit def enrichDListWithSampleBy[A : Manifest : WireFormat, B : Manifest : WireFormat](x: DList[(A,B)])(implicit hashable: Hashable[A,Bits32]) =
    DListWithSampleBy(x)

  implicit def enrichDListWithLimit[A : Manifest : WireFormat](x: DList[A]) =
    DListWithLimit(x)

  implicit def enrichDListWithPartitionAtRandom[A : Manifest : WireFormat](x: DList[A]) =
    DListWithPartitionAtRandom(x)

  implicit def enrichDListWithBloomJoin[A : Manifest : WireFormat : Grouping, BL : Manifest : WireFormat](x: DList[(A, BL)])(implicit hashable: Hashable[A,Bits32]) =
    DListWithBloomJoin(x)

  implicit def enrichDListWithSkewedJoin[A : Manifest : WireFormat : Grouping, BL : Manifest : WireFormat](x: DList[(A, BL)])(implicit hashable: Hashable[A,Bits32]) =
    DListWithSkewedJoin(x)

}

object implicits extends ImplicitConversions
