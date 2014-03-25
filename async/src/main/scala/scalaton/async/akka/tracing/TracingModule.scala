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

package scalaton.async.akka.tracing

import akka.actor._
import com.github.nscala_time.time.Imports._
import scalaton.async.akka._

trait TracingModule extends Akka {
  import TraceAggregator._

  implicit class TracingOps(receiver: ActorRef) {
    def !+[A](msg: A)(implicit sender: ActorRef = Actor.noSender): Unit = {
      traceAggregator ! SendTrace(Option(sender), receiver, System.currentTimeMillis, msg)
      receiver ! msg
    }

    def tell_+[A](msg: A, sender: ActorRef) = !+(msg)(sender)

    def >+[A](msg: A)(implicit context: ActorContext): Unit = {
      traceAggregator ! ForwardTrace(Option(context.sender), receiver, context.self, System.currentTimeMillis, msg)
      receiver forward msg
    }

  }

  trait TracingActor extends Actor {
    def traceReceivedMessage(msg: Any, sender: ActorRef) =
      traceAggregator.tell(ReceiveTrace(Option(sender), self, System.currentTimeMillis, msg), self)

    def tracedReceive(recv: Receive): Receive = {
      val tr: PartialFunction[Any,Any] = {
        case msg =>
          traceReceivedMessage(msg, sender)
          msg
      }

      tr.andThen(recv)
    }
  }

  lazy val traceAggregator: ActorRef = system.actorOf(Props[TraceAggregator], "trace-aggregator")
}

object TraceAggregator {
  case class SendTrace[A](val sender: Option[ActorRef], val receiver: ActorRef, val timestamp: Long , val msg: A) {
    def prettify = {
      // val t = (new DateTime(System.currentTimeMillis)).toString(org.joda.time.format.ISODateTimeFormat.dateTime)
      val s = sender.map(_.path.name).getOrElse("<NA>")
      s"SEND    | $timestamp | sender:$s | receiver:${receiver.path.name} | $msg"
    }
  }

  case class ReceiveTrace(val sender: Option[ActorRef], val receiver: ActorRef, val timestamp: Long, val msg: Any) {
    def prettify = {
      val s = sender.map(_.path.name).getOrElse("<NA>")
      s"RECEIVE | $timestamp | sender:${s} | receiver:${receiver.path.name} | $msg"
    }
  }

  case class ForwardTrace[A](val sender: Option[ActorRef], val receiver: ActorRef, val through: ActorRef, val timestamp: Long , val msg: A) {
    def prettify = {
      val s = sender.map(_.path.name).getOrElse("<NA>")
      s"FORWARD | $timestamp | sender:${s} | through:${through.path.name} | receiver:${receiver.path.name} | $msg"
    }
  }
}
class TraceAggregator extends Actor with ActorLogging {
  import TraceAggregator._

  val receive: Receive = {
    case st: SendTrace[_] =>
      log.debug(st.prettify)

    case rt: ReceiveTrace =>
      log.debug(rt.prettify)

    case ft: ForwardTrace[_] =>
      log.debug(ft.prettify)
  }
}

/*
import scalaton.async.akka.tracing._
import akka.actor._
import scala.concurrent._, duration._

lazy val sys = ActorSystem("system")

object Main extends TracingModule {
 lazy val system = sys

 val a = system.actorOf(Props(new TracingActor { val receive: Receive = tracedReceive({ case msg => println(("forward", msg)); c >+ msg })}))
 val b = system.actorOf(Props(new Actor { val receive: Receive = { case _ =>  } }))
 val c = system.actorOf(Props(new TracingActor { val receive: Receive = tracedReceive({ case msg => println(("echo", msg)) })}))
}
import Main._

c !+ "hello"
a !+ "hello"

*/
