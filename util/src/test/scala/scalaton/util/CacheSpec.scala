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

class CacheSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  import mutable._

  behavior of "an LRU cache"

  it should "hold recently used items" in {
    forAll{
      (xs: List[(String,Int)], n: Int) => whenever(n >= 0){
        val lru = new LruCache[String,Int](n)

        xs.foreach{ case (x, k) => lru.update(x, k) }

        val actual = lru.keySet.map(x => (x, lru(x)))
        val expected = xs.takeRight(n).foldLeft(Map[String,Int]()){ case (accum, xk) => accum + xk }.toSet

        actual should be(expected)
      }
    }
  }

  behavior of "an expiring LRU cache"

  it should "hold recently used items before expiration" in {
    val lru = new ExpiringLruCache[String,Int](100, 16, 1000, 1000, 0, 1000, 0.9)

    lru.update("a", 1)
    lru.update("b", 2)
    lru.update("c", 3)

    (lru.get("a")) should be(Some(1))
    (lru.get("b")) should be(Some(2))
    (lru.get("c")) should be(Some(3))

    Thread.sleep(1100)

    (lru.get("a")) should be(None)
    lru.keySet should be(Set("b", "c"))

    lru.update("d", 4)

    lru.keySet should be(Set("d"))
  }

  it should "clean up expired items on update" in {
    val lru = new ExpiringLruCache[String,Int](100, 16, 1000, 1000, 0, 2000, 0.9)

        lru.update("a", 1)
        lru.update("b", 2)
        lru.update("c", 3)

        (lru get "a") should be(Some(1))
        (lru get "b") should be(Some(2))
        (lru get "c") should be(Some(3))

        Thread.sleep(1100)

        (lru get "a") should be(None)
        lru.keySet should be(Set("b", "c"))

        lru.update("d", 4)

        lru.keySet should be(Set("b", "c", "d"))

        Thread.sleep(1100)
        lru.update("d", 5)

        lru.keySet should be(Set("d"))
      }

}

