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

package scalaton.async.actor.tracing

import akka.actor._
import com.github.nscala_time.time.Imports._
import scalaton.async.actor._
import scalaz._, Scalaz._
import scala.collection.mutable

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

  val turnOnTracing = true
  val tracingStatsBufferSize = 2
  val tracingTimelineSize = 100
  val traceAggregator: ActorRef = system.actorOf(Props(new TraceAggregator(tracingStatsBufferSize, tracingTimelineSize, turnOnTracing)), "trace-aggregator")
}

object TraceAggregator {
  case object TurnOnTracing
  case object TurnOffTracing

   private[tracing] sealed trait MessageTrace[A] {
    val sender: Option[ActorRef]
    val receiver: ActorRef
    val timestamp: Long
    val msg: A
    val msgType: String
    val traceType: String
    def prettify: String
  }

  case class SendTrace[A] private[tracing](val sender: Option[ActorRef], val receiver: ActorRef, val timestamp: Long , val msg: A, val msgTypeOption: Option[String] = None) extends MessageTrace[A] {
    lazy val msgType = msgTypeOption.getOrElse(msg.getClass.getName)
    val traceType = "send"

    def prettify = {
      val s = sender.map(_.path.toString).getOrElse("akka://system/deadLetters")
      s"| trace:SEND | timestamp:$timestamp | sender:$s | receiver:${receiver.path.toString} | $msg"
    }
  }

  case class ReceiveTrace private[tracing](val sender: Option[ActorRef], val receiver: ActorRef, val timestamp: Long, val msg: Any, val msgTypeOption: Option[String] = None) extends MessageTrace[Any] {
    lazy val msgType = msgTypeOption.getOrElse(msg.getClass.getName)
    val traceType = "receive"
    def prettify = {
      val s = sender.map(_.path.toString).getOrElse("akka://system/deadLetters")
      s"| trace:RECEIVE | timestamp:$timestamp | sender:${s} | receiver:${receiver.path.toString} | $msg"
    }
  }

  case class ForwardTrace[A] private[tracing](val sender: Option[ActorRef], val receiver: ActorRef, val through: ActorRef, val timestamp: Long , val msg: A, val msgTypeOption: Option[String] = None) extends MessageTrace[A] {
    lazy val msgType = msgTypeOption.getOrElse(msg.getClass.getName)
    val traceType = "forward"

    def prettify = {
      val s = sender.map(_.path.toString).getOrElse("akka://system/deadLetters")
      s"| trace:FORWARD | timestamp:$timestamp | sender:${s} | through:${through.path.toString} | receiver:${receiver.path.toString} | $msg"
    }
  }
}
class TraceAggregator(tracingStatsBufferSize: Int, tracingTimelineSize: Int, turnOnTracing: Boolean) extends Actor with ActorLogging {
  import TraceAggregator._

  var on = turnOnTracing

  val stats = context.actorOf(Props(new TraceStats(tracingStatsBufferSize, tracingTimelineSize, turnOnTracing)), "trace-stats")

  val receive: Receive = {
    ({
      case TurnOnTracing =>
        log.debug("turn on tracing")
        on = true
        stats ! TraceStats.TurnOn
      case TurnOffTracing =>
        log.debug("turn off tracing")
        on = false
        stats ! TraceStats.TurnOff
      case _ if !on => log.warning("tracing is turned off")
    }: Receive) orElse {
      case messageTrace: MessageTrace[_] =>
        log.info(messageTrace.prettify)
        stats ! messageTrace
    }}
}

object TraceStats {
  import TraceAggregator._

  private[tracing] case object TurnOn
  private[tracing] case object TurnOff

  case object GetOverallMessageCounts
  case class OverallMessageCounts(val counts: Map[String,Map[String,Long]])

  case object GetBufferedMessageTraces
  case class BufferedMessageTraces(val counts: Seq[MessageTrace[_]])

  case class GetTimeline(val format: TimelineFmt)
  case class TimelineTable(val columns: Vector[String], val table: Vector[Vector[Option[MessageTrace[_]]]])
  case class TimelineAscii(val table: String)

  sealed trait TimelineFmt
  case object TimelineTableFormat extends TimelineFmt
  case object TimelineAsciiFormat extends TimelineFmt

}
class TraceStats(tracingStatsBufferSize: Int, tracingTimelineSize: Int, turnOnTracing: Boolean) extends Actor with ActorLogging {
  import TraceAggregator._
  import TraceStats._

  var on = turnOnTracing
  val buf = mutable.Queue.empty[MessageTrace[_]]

  def overallMessageCounts =
    buf.foldLeft(Map.empty[String,Map[String,Long]])((counts, m) => counts |+| Map(m.msgType -> Map(m.traceType -> 1)))

  def timeline = {
    val byTime = buf.take(tracingTimelineSize).toList.map{
      case mt@SendTrace(sender, receiver, timestamp, msg, _) => (sender.map(_.path.toString).getOrElse("akka://system/deadLetters"), mt)
      case mt@ReceiveTrace(sender, receiver, timestamp, msg, _) => (receiver.path.toString, mt)
      case mt@ForwardTrace(sender, receiver, through, timestamp, msg, _) => (through.path.toString, mt)
    }.sortBy(_._2.timestamp)
    val cols = byTime.map(_._1).toSet.toVector.sorted
    val colnum = cols.zipWithIndex.toMap
    val n = colnum.size
    val table = byTime.foldLeft(Vector.empty[(DateTime,Vector[Option[MessageTrace[_]]])]){ case (t, (a, mt)) =>
      t :+ (new DateTime(mt.timestamp), Vector.fill(n)(none[MessageTrace[_]]).updated(colnum(a), mt.some))
    }

    (cols, table)
  }

  lazy val receive: Receive = {
    ({
      case TurnOn =>
        log.debug("turn on tracing")
        on = true
      case TurnOff =>
        log.debug("turn off tracing")
        on = false
      case _ if !on=> log.warning("tracing is turned off")
    }: Receive) orElse processMessage orElse handleRequest
  }

  val processMessage: Receive = {
    case messageTrace: MessageTrace[_] =>
      buf.enqueue(messageTrace)
      if (buf.size > tracingStatsBufferSize)
        buf.dequeue
  }

  val handleRequest: Receive = {
    case GetOverallMessageCounts => sender ! OverallMessageCounts(overallMessageCounts)
    case GetBufferedMessageTraces => sender ! BufferedMessageTraces(buf)
    case GetTimeline(fmt) =>
      val (columns, table) = timeline

      fmt match {
        case TimelineTableFormat => sender ! TimelineTable(columns, table.map(_._2))
        case TimelineAsciiFormat =>
          val xs = ("time \\ actor" +: columns) +: table.view.map{
            case (datetime, rows) => datetime.toString(org.joda.time.format.ISODateTimeFormat.dateTime) +: rows.map(row => row.fold(""){
              case mt@SendTrace(sender, receiver, timestamp, msg, _) => s"S ${msg.toString} -> ${receiver.path.toString}"
              case mt@ReceiveTrace(sender, receiver, timestamp, msg, _) => val sdr = sender.map(_.path.toString).getOrElse("akka://system/deadLetters") ; s"R ${msg.toString} <- $sdr"
              case mt@ForwardTrace(sender, receiver, through, timestamp, msg, _) => val sdr = sender.map(_.path.toString).getOrElse("akka://system/deadLetters"); s"F ${msg.toString} -> ${receiver.path.toString}"
            })
          }

          val longestSizes = xs.foldLeft(Vector.fill(columns.size + 1)(0)){ (szs, row) =>
            szs.zip(row.map(_.size)).map{ case (a,b) => a.max(b) }
          }


          val ys = xs.map{ row =>
            row.zip(longestSizes).map{ case (s, longestSize) =>
              val pad = (" " * (longestSize - s.size))

              s + pad
            }.mkString(" | ")
          }

          val ascii = (Vector(ys.head, "-" * ys.head.size) ++ ys.tail).mkString("\n")

          sender ! TimelineAscii(ascii)
      }

  }
}


object SampleTracingRun extends App {
  import scala.concurrent._, duration._, ExecutionContext.Implicits.global
  import akka.pattern.ask

  case object Foo
  lazy val sys = ActorSystem("system")

  object Main extends TracingModule {
    override val turnOnTracing = true
    lazy val system = sys

    val a = system.actorOf(Props(new TracingActor { val receive: Receive = tracedReceive({ case msg => println(("forward", msg)); c >+ msg })}), "AA")
    val b = system.actorOf(Props(new Actor { val receive: Receive = { case _ =>  } }), "BB")
    val c = system.actorOf(Props(new TracingActor { val receive: Receive = tracedReceive({ case msg => println(("echo", msg)) })}), "CC")
  }
  import Main._

  implicit val timeout = akka.util.Timeout(DurationInt(1).seconds)
  val traceStats = system.actorSelection("akka://system/user/trace-aggregator/trace-stats")

  c !+ "hello"

  Await.result(traceStats ? TraceStats.GetOverallMessageCounts, Duration.Inf).asInstanceOf[TraceStats.OverallMessageCounts].counts.foreach(println)

  traceAggregator ! TraceAggregator.TurnOffTracing

  a !+ "hello - untraced"

  traceAggregator ! TraceAggregator.TurnOnTracing

  a !+ "hello"

  Await.result(traceStats ? TraceStats.GetOverallMessageCounts, Duration.Inf).asInstanceOf[TraceStats.OverallMessageCounts].counts.foreach(println)

  c !+ Foo

  Await.result(traceStats ? TraceStats.GetOverallMessageCounts, Duration.Inf).asInstanceOf[TraceStats.OverallMessageCounts].counts.foreach(println)

  println("------------------------")

  val timeline = Await.result(traceStats ? TraceStats.GetTimeline(TraceStats.TimelineAsciiFormat), Duration.Inf).asInstanceOf[TraceStats.TimelineAscii]
  println(timeline.table)

  akka.pattern.after(DurationInt(5).seconds, system.scheduler)(Future(system.shutdown))
}
