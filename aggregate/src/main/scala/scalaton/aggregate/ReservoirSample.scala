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

package scalaton.aggregate

import collection.mutable

import scalaz._
import Scalaz._

trait ReservoirSampleModule{

  def reservoirSample[A](n: Int, seed: Int = 0): Iterable[A] => Vector[A] = (xs: Iterable[A]) => {
    val buf = new mutable.ArrayBuffer[A](n)
    val reservoir = buf.mapResult(_.toVector)
    val rand = new util.Random(seed)


    for((x, i) <- xs.zipWithIndex){
      if(i < n){
        buf += x
      }else if(rand.nextInt(i) lt n){
        buf(rand.nextInt(n)) = x
      }
    }

    reservoir.result()
  }

}


object rsmpl extends ReservoirSampleModule
