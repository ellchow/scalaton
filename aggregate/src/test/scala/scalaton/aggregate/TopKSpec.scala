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

import org.specs2.mutable._
import scalaz._
import Scalaz._

class TopKSpec extends Specification{
  "TopKByScore" should {
    "keep top k items" in {
      for(_ <- 1 to 100)
      yield{
        val k = util.Random.nextInt(20)

        trait TK
        implicit val tk = topk[String, Int, TK](k)

        val items = (1 to 100).map(_.toString).zip(util.Random.shuffle(1 to 100))

        topk.fromData(items).toList must_== items.sortBy(_._2).takeRight(k).toList
      }
    }

    "should merge and keep top k" in {
      for(_ <- 1 to 100)
      yield{
        val k = util.Random.nextInt(20)

        trait TK
        implicit val tk = topk[String, Int, TK](k)

        val items = (1 to 100).map(_.toString).zip(util.Random.shuffle(1 to 100))
        val n = util.Random.nextInt(items.size)



        val x = (topk.fromData(items.take(n)) |+| topk.fromData(items.drop(n))).toList
        val y = items.sortBy(_._2).takeRight(k).toList

        x must_== y
      }
    }


  }

}
