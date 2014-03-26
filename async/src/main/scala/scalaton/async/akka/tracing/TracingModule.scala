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
import scalaz._, Scalaz._

trait TracingModule extends Akka {
  import TraceAggregator._

  implicit class TracingOps(receiver: ActorRef) {
    def !+[A](msg: A)(implicit sender: ActorRef = Actor.noSender): Unit = {
      if (turnOnTracing)
        traceAggregator ! SendTrace(Option(sender), receiver, System.currentTimeMillis, msg)
      receiver ! msg
    }

    def tell_+[A](msg: A, sender: ActorRef) = !+(msg)(sender)

    def >+[A](msg: A)(implicit context: ActorContext): Unit = {
      if (turnOnTracing)
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

  def turnOnTracing = true
  lazy val traceAggregator: ActorRef = system.actorOf(Props[TraceAggregator], "trace-aggregator")
}

object TraceAggregator {
  sealed trait MessageTrace[A] {
    val sender: Option[ActorRef]
    val receiver: ActorRef
    val timestamp: Long
    val msg: A
    val msgType: String
    val traceType: String
    def prettify: String
  }

  case class SendTrace[A](val sender: Option[ActorRef], val receiver: ActorRef, val timestamp: Long , val msg: A, val msgTypeOption: Option[String] = None) extends MessageTrace[A] {
    lazy val msgType = msgTypeOption.getOrElse(msg.getClass.getName)
    val traceType = "send"

    def prettify = {
      // val t = (new DateTime(System.currentTimeMillis)).toString(org.joda.time.format.ISODateTimeFormat.dateTime)
      val s = sender.map(_.path.name).getOrElse("<NA>")
      s"| TRACE:SEND | $timestamp | sender:$s | receiver:${receiver.path.name} | $msg"
    }
  }

  case class ReceiveTrace(val sender: Option[ActorRef], val receiver: ActorRef, val timestamp: Long, val msg: Any, val msgTypeOption: Option[String] = None) extends MessageTrace[Any] {
    lazy val msgType = msgTypeOption.getOrElse(msg.getClass.getName)
    val traceType = "receive"
    def prettify = {
      val s = sender.map(_.path.name).getOrElse("<NA>")
      s"| TRACE:RECEIVE | $timestamp | sender:${s} | receiver:${receiver.path.name} | $msg"
    }
  }

  case class ForwardTrace[A](val sender: Option[ActorRef], val receiver: ActorRef, val through: ActorRef, val timestamp: Long , val msg: A, val msgTypeOption: Option[String] = None) extends MessageTrace[A] {
    lazy val msgType = msgTypeOption.getOrElse(msg.getClass.getName)
    val traceType = "forward"

    def prettify = {
      val s = sender.map(_.path.name).getOrElse("<NA>")
      s"| TRACE:FORWARD | $timestamp | sender:${s} | through:${through.path.name} | receiver:${receiver.path.name} | $msg"
    }
  }
}
class TraceAggregator extends Actor with ActorLogging {
  import TraceAggregator._

  val stats = context.actorOf(Props(new TraceStats), "trace-stats")

  val receive: Receive = {
    case messageTrace: MessageTrace[_] =>
      log.info(messageTrace.prettify)
      stats ! messageTrace
  }
}

object TraceStats {
  case object GetOverallMessageCounts
  case class OverallMessageCounts(val counts: Map[String,Map[String,Long]])
}
class TraceStats extends Actor with ActorLogging {
  import TraceAggregator._
  import TraceStats._

  var overallMessageCounts = Map.empty[String,Map[String,Long]] // message type -> trace type -> Long

  lazy val receive: Receive = processMessage orElse handleRequest

  val processMessage: Receive = {
    case messageTrace: MessageTrace[_] =>
      overallMessageCounts = overallMessageCounts |+| Map(messageTrace.msgType -> Map(messageTrace.traceType -> 1))
  }

  val handleRequest: Receive = {
    case GetOverallMessageCounts => sender ! OverallMessageCounts(overallMessageCounts)
  }
}
