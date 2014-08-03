/*
 Copyright 2014 Elliot Chow

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

import com.typesafe.scalalogging.slf4j._
import scalaz._, Scalaz._

object LoggingExtensions extends Logging {
  object loglevel extends Enumeration {
    val info, warn, error, debug, trace = Value
  }
  implicit class DisjThrowableOps[A](disj: Throwable \/ A) {
    def logged(lvl: loglevel.Value = loglevel.error) = {
      disj.leftMap{ t => lvl match {
        case loglevel.info => logger.info(t.stackTrace)
        case loglevel.warn => logger.warn(t.stackTrace)
        case loglevel.error => logger.error(t.stackTrace)
        case loglevel.debug => logger.debug(t.stackTrace)
        case loglevel.trace => logger.trace(t.stackTrace)

      }}
      disj
    }

    def toOptionLogged(lvl: loglevel.Value = loglevel.error) =
      logged(lvl).toOption

    def foreachLogged(f: A => Unit, lvl: loglevel.Value = loglevel.error) =
      toOptionLogged(lvl).foreach(f)

    def rethrow(lvl: loglevel.Value = loglevel.error) =
      logged(lvl).fold(t => throw t, identity)
  }

}
