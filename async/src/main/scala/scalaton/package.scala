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
import java.util.concurrent.Executors
import scalaz._, Scalaz._
import scala.util.{ Try, Success, Failure }
import io.netty.util._

package object async {
  object Implicits {
    implicit lazy val defaultHashedWheelTimer = new HashedWheelTimer
    implicit lazy val defaultExecutionContext = ExecutionContext.Implicits.global
  }

  implicit class ExecutionContextOps(val x: ExecutionContext.type) extends AnyVal {
    def fixedThreadPool(n: Int) =
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(n))

    def forkJoinPool(n: Int) =
      ExecutionContext.fromExecutorService(new forkjoin.ForkJoinPool(n))

    def cachedThreadPool =
      ExecutionContext.fromExecutorService(Executors.newCachedThreadPool)
  }

  implicit class FutureCompanionOps(val companion: Future.type) extends AnyVal {
    def never[A]: Future[A] = Promise[A]().future

    def waitFor(t: Duration)(implicit hashedWheelTimer: HashedWheelTimer): Future[Unit] = {
      val p = Promise[Unit]()

      hashedWheelTimer.newTimeout(new TimerTask { def run(timeout: Timeout){ p.complete(Try(Unit)) } },
        t.toMillis,
        java.util.concurrent.TimeUnit.MILLISECONDS)

      p.future
    }

    def firstCompletedOf[A,B](fa: Future[A], fb: Future[B])(implicit executionContext: ExecutionContext): Future[A \/ B] = {
      val p = Promise[A \/ B]()

      fa.onComplete(a => p.tryComplete(a.map(_.left)))
      fb.onComplete(b => p.tryComplete(b.map(_.right)))

      p.future
    }

    def timeout[A](f: Future[A], t: Duration)(implicit executionContext: ExecutionContext, hashedWheelTimer: HashedWheelTimer): Future[A] = {
      val result = firstCompletedOf(waitFor(t).map(_ => new TimeoutException), f)

      result.map(_.fold(e => throw e, identity))
    }

    def continueWith[A,B](f: Future[A], cont: Future[A] => B)(implicit executionContext: ExecutionContext): Future[B] = {
      val p = Promise[B]()
      f.onComplete(_ => p.complete(Try(cont(f))))
      p.future
    }


    def continue[A,B](f: Future[A], cont: Try[A] => B)(implicit executionContext: ExecutionContext): Future[B] = {
      val p = Promise[B]()
      f.onComplete(t => p.complete(Try(cont(t))))
      p.future
    }

    def retry[A](f: =>Future[A], timeoutDurations: List[Duration], retryOn: Throwable => Boolean = _ => true)(implicit executionContext: ExecutionContext, hashedWheelTimer: HashedWheelTimer): Future[A] = {
      require(timeoutDurations.nonEmpty, "timeoutDurations size must be > 0")

      lazy val cont: Try[A] => Future[A] = {
        def again(throwable: Throwable) = timeoutDurations match {
          case _ :: ts if ts.nonEmpty => retry(f, ts)
          case _ => throw throwable
        }

        {
          case Success(a) => Future.successful(a)
          case Failure(throwable: TimeoutException) => again(throwable)
          case Failure(throwable) if retryOn(throwable) => again(throwable)
          case Failure(throwable) => throw throwable
        }
      }

      continue(timeout(f, timeoutDurations.head), cont).flatMap(identity)
    }

    def schedule[A](f: =>Future[A], t: Duration)(implicit executionContext: ExecutionContext, hashedWheelTimer: HashedWheelTimer) = waitFor(t).flatMap(_ => f)
  }

  private lazy val futureCompanionOps = new FutureCompanionOps(Future)

  implicit class FutureOps[A](val f: Future[A]) extends AnyVal {
    def now: A = f.value.map{
      case Success(t) => t
      case Failure(e) => throw e
    }.getOrElse(throw new NoSuchElementException)

    def or[B](g: Future[B])(implicit executionContext: ExecutionContext) =
      futureCompanionOps.firstCompletedOf(f,g)

    def timeoutAfter(t: Duration)(implicit executionContext: ExecutionContext, hashedWheelTimer: HashedWheelTimer) =
      futureCompanionOps.timeout(f,t)

    def continueWith[B](cont: Future[A] => B)(implicit executionContext: ExecutionContext): Future[B] =
      futureCompanionOps.continueWith(f, cont)

    def continue[B](cont: Try[A] => B)(implicit executionContext: ExecutionContext): Future[B] =
      futureCompanionOps.continue(f, cont)
  }
}
