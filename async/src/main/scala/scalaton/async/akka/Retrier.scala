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

package scalaton.async.akka

import akka.actor._
import scala.concurrent._, duration._
import scala.util.{ Try, Success, Failure }
import scala.collection.mutable

object Retrier {
  case class RetrierTimeout(val id: Long) extends Exception
  case class Result(val id: Long, val value: Try[Any])
  case object GetResult
}

/** generic retrying actor - execute function `run`, which should send a message of type Try[Any] containing the result to the retrier itself
    Example:
      context.actorOf(Props(new Retrier(123, List.fill(3)(2.seconds)){ def run() = Future(self ! Try("do something here"))(ExecutionContext.Implicits.global) }))
*/
abstract class Retrier(id: Long, timeouts: List[FiniteDuration], autoStart: Option[FiniteDuration] = Some(0.millis)) extends Actor with ActorLogging {
  import Retrier._

  def run(): Unit

  case object Go

  val responders: Set[ActorRef] = Set(self)

  val receive: Receive = {
    autoStart.fold(self ! Go)(d => context.system.scheduler.scheduleOnce(d, self, Go)(context.dispatcher))
    attempting(timeouts)
  }

  def attempting(remainingTimeouts: List[FiniteDuration]): Receive = {
    case Go =>
      remainingTimeouts match {
        case t :: ts =>
          log.debug("attempt {} of {}", timeouts.size - ts.size, timeouts.size)
          context.system.scheduler.scheduleOnce(t, self, Go)(context.dispatcher)
          context.become(attempting(ts))
          run()

        case Nil =>
          notifyParentAndFinish(Failure(RetrierTimeout(id)))
      }

    case s: Success[_] if responders.contains(sender) =>
      notifyParentAndFinish(s)

    case f: Failure[_] if responders.contains(sender) =>
      remainingTimeouts match {
        case t :: _ => self ! Go

        case Nil => notifyParentAndFinish(f)
      }

  }

  def gotResult(value: Try[Any]): Receive = {
    case GetResult =>
      sender ! Result(id, value)
  }

  def notifyParentAndFinish(value: Try[Any]): Unit = {
    context.parent ! Result(id, value)
    context.become(gotResult(value))
  }

}

class FutureRetrier(id: Long, block: =>Any, timeouts: List[FiniteDuration], autoStart: Option[FiniteDuration] = Some(0.millis))(implicit executionContext: ExecutionContext) extends Retrier(id, timeouts, autoStart) {

  def run() = Future(block).onComplete(res => self ! (id, res))

}

class UniformRetrier(create: PartialFunction[Any,Retrier], maxWorkers: Int = 5) extends Actor with ActorLogging {
  import UniformRetrier._

  val buf = mutable.Queue.empty[() => (ActorRef, ActorRef)] // (requester, retrier)

  var requests = Map.empty[ActorRef, ActorRef] // retrier -> requester

  val receive: Receive = {
    case a if create.isDefinedAt(a) =>
      val requester = sender
      buf.enqueue(() => (requester, context.actorOf(Props(create(a)))))
      runNext()

    case result: Retrier.Result =>
      requests.get(sender) match {
        case Some(requester) =>
          requester ! result
          context.stop(sender)
        case None =>
          runNext()
      }

    case Terminated(retrier) =>
      requests = requests - retrier
      runNext()
  }

  def runNext(): Unit = {
    if (requests.size < maxWorkers && buf.nonEmpty) {
      val init = buf.dequeue
      val (requester, retrier) = init()
      requests = requests + (retrier -> requester)
      context.watch(retrier)
      runNext()
    }
  }
}



/*
import scalaton.async.akka._
import akka.actor._
import scala.util.{ Try, Success, Failure }
import scala.concurrent._, duration._
import scala.concurrent.ExecutionContext.Implicits.global

val system = ActorSystem("system")
val c = system.actorOf(Props(new UniformRetrier({ case x: Int => new FutureRetrier(x, { println(("attempt", x)); Thread.sleep(100); val y = scala.util.Random.nextDouble ; if (y < 0.5) throw new Exception(":(") else "ok " + x }, List(1.seconds,1.seconds)) }: PartialFunction[Any,Retrier])))
val d = system.actorOf(Props(new Actor {  val receive: Receive = { case x => println(("done", x)) } }), "FOO")
(1 to 10).foreach(i => c.tell(i,d))



val a = system.actorOf(Props(new Actor {
  override val supervisorStrategy = OneForOneStrategy() {
    case _ => SupervisorStrategy.Stop
  }

  def b = context.actorOf(Props(new FutureRetrier(123, {
    val res = if (scala.util.Random.nextDouble < 0.5) 123123 else throw new Exception(":(" )
    if (scala.util.Random.nextDouble < 0.1) Thread.sleep(10000)
     res
   }, List.fill(3)(2.seconds))(context.dispatcher) {}), "my-retrier")

  val receive: Receive = {
    case Retrier.Result(id, Success(x: Any)) => println(("Success!", id, x))
    case Retrier.Result(id, Failure(e: Throwable)) => println(("Failure...", id, e))
  }

  b
}), "my-actor" + System.currentTimeMillis)




*/
