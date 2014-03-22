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

import com.github.nscala_time.time.Imports._
import scalaz._, Scalaz._
import moments._

object timer {
  def startTimer(label: String) = Timer(label)

  case class Timer(label: String , timestamp: Long = System.currentTimeMillis, moments: Moments = implicitly[Monoid[Moments]].zero) {
    def tick = {
      val now = System.currentTimeMillis
      val delta = (now - timestamp)

      Timer(label, now, moments |+| Moments(delta))
    }

    override def toString = f"Timer($label%s: ${1000.0/moments.mean}%.2f per second)"
  }
}
