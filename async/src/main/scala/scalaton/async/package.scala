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

package scalaton

import scala.concurrent._, duration._
import rx.lang.scala._
import play.api.libs.iteratee._
import scala.util.{ Try, Success, Failure }

package object async extends ExecutionContextExtensions with FutureExtensions {

  /** blatantly copied from https://github.com/gilbertw1/rxplay-example */
  implicit class PlayEnumeratorToObservable[T](enum: Enumerator[T]) {
    def toObservable(implicit executionContext: ExecutionContext) = {
      Observable{ observer: Observer[T] =>
        var cancelled = false
        val cancellableEnum = enum through Enumeratee.breakE[T](_ => cancelled)

        cancellableEnum (
          Iteratee.foreach(observer.onNext(_))
        ).onComplete {
          case Success(_) => observer.onCompleted()
          case Failure(e) => observer.onError(e)
        }

        new Subscription { override def unsubscribe() = { cancelled = true } }
      }
    }
  }
}
