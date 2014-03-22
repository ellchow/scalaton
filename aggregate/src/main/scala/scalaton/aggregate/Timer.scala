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
import com.typesafe.scalalogging.slf4j._

object timer {
  def startTimer(label: String) = new Timer(label, System.currentTimeMillis)

  def startLoggingTimer(label: String, trigger: (Long,Long,Moments) => Boolean) = {
    val t = trigger.curried(System.currentTimeMillis)

    new LoggingTimer(label, t, System.currentTimeMillis)
  }

  class Timer(label: String , timestamp: Long, moments: Moments = implicitly[Monoid[Moments]].zero) {
    def tick = {
      val now = System.currentTimeMillis
      val delta = (now - timestamp)

      new Timer(label, now, moments |+| Moments(delta))
    }

    def info = f"$label%s: rate=${1000.0/moments.mean}%.6f/s, count=${moments.n}"

    override def toString = s"Timer($info)"
  }

  class LoggingTimer(label: String, trigger: Long => Moments => Boolean, timestamp: Long, moments: Moments = implicitly[Monoid[Moments]].zero)
      extends Timer(label, timestamp, moments) with Logging {

    override def tick = {
      val now = System.currentTimeMillis
      val delta = (now - timestamp).max(0)
      val m = moments |+| Moments(delta)

      val t = new LoggingTimer(label, trigger, now, m)
      if (trigger(now)(m)) logger.info(t.info)
      t
    }

  }

}
