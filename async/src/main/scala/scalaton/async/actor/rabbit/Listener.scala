package scalaton.async.actor.rabbit

import akka.actor._, SupervisorStrategy._
import com.rabbitmq.client._
import scala.collection.JavaConversions._
import scala.concurrent.{ Future, ExecutionContext }
import scala.concurrent.duration._
import scalaton.async.actor._
import scalaton.async.rabbit._
import scalaton.util._
import scalaton.util.Json._
import scalaz._, Scalaz._
import argonaut._, Argonaut._
import java.util.UUID
import scala.util.{ Try, Success, Failure }

object Listener {
  import Amqp._

  case class Delivery[A : DecodeJson] private[rabbit] (delivery: QueueingConsumer.Delivery) {
    lazy val message = new String(delivery.getBody).decodeEither[A]
    def tag = delivery.getEnvelope.getDeliveryTag
    def exchange = delivery.getEnvelope.getExchange
    def routingKey = delivery.getEnvelope.getRoutingKey
    def redelivered = delivery.getEnvelope.isRedeliver
    override def toString = s"Delivery($exchange, $routingKey, $tag)"
  }

  sealed trait ProcessStatus[A]{
    val delivery: Delivery[A]
  }
  case class Ack[A](delivery: Delivery[A], multiple: Boolean = false) extends ProcessStatus[A]
  case class Nack[A](delivery: Delivery[A], requeue: Boolean, multiple: Boolean = false) extends ProcessStatus[A]
  case class Reject[A](delivery: Delivery[A], requeue: Boolean) extends ProcessStatus[A]

  def props[A](
    rabbitManager: ActorRef,
    queue: Queue,
    consumerTag: ConsumerTag = ConsumerTag(UUID.randomUUID.toString),
    autoAck: Boolean = false,
    exclusive: Boolean = false,
    arguments: Map[String,java.lang.Object] = Map.empty,
    noLocal: Boolean = false,
    ec: ExecutionContext = ExecutionContext.Implicits.global
  )(f: Delivery[A] => ProcessStatus[A])(implicit dec: DecodeJson[A]) =
    Props(new Listener[A](rabbitManager, queue, consumerTag, autoAck, exclusive, arguments, noLocal, ec){
      def processDelivery(d: Delivery[A]) = f(d)
    })
}

abstract class Listener[A : DecodeJson](
  rabbitManager: ActorRef,
  queue: Amqp.Queue,
  consumerTag: Amqp.ConsumerTag = Amqp.ConsumerTag(UUID.randomUUID.toString),
  autoAck: Boolean = false,
  exclusive: Boolean = false,
  arguments: Map[String,java.lang.Object] = Map.empty,
  noLocal: Boolean = false,
  ec: ExecutionContext = ExecutionContext.Implicits.global
) extends Actor with ActorLogging {
  import Amqp._
  import Manager._
  import Listener._

  private var channel: Option[Channel] = None
  private var consumer: Option[QueueingConsumer] = None

  override def preStart(): Unit = {
    context.system.scheduler.scheduleOnce(500.millis, rabbitManager, GetChannel)(context.dispatcher, self)
  }

  def setChannel: Receive = {
    case SetChannel(ch) =>
      log.info(s"setting rabbit channel to $ch")
      channel = ch.some
      makeConsumer()
      awaitDelivery()
      context.become(listening)

  }

  lazy val receive: Receive = idle

  def idle = setChannel

  def listening = setChannel.orElse({
    case Success(d: Delivery[A]) =>
      Future{
        processDelivery(d) match {
          case Ack(d, multiple) =>
            log.debug(s"ack-ing delivery ${d.tag}")
            channel.foreach(_.basicAck(d.tag, multiple))
          case Nack(d, requeue, multiple) =>
            log.debug(s"nack-ing delivery ${d.tag}")
            channel.foreach(_.basicNack(d.tag, multiple, requeue))
          case Reject(d, requeue) =>
            log.debug(s"rejecting delivery ${d.tag}")
            channel.foreach(_.basicReject(d.tag, requeue))
        }
        awaitDelivery()
      }(ec)
        .onFailure{ case t: Throwable => self ! Failure(t)}(ec)

    case Failure(e: ShutdownSignalException) =>
      log.error(e.stackTrace)
      context.become(idle)

    case Failure(e) =>
      log.error(e.stackTrace)
      rabbitManager ! GetChannel
      awaitDelivery()

  }: Receive)

  def processDelivery(d: Delivery[A]): ProcessStatus[A]

  def awaitDelivery(): Unit =
    consumer.foreach{ qc =>
      Future(Delivery[A](qc.nextDelivery))(ec).onComplete{ res => self ! res }(ec)
    }


  def makeConsumer(): Unit = {
    log.info(s"setting up consumer $consumerTag of queue $queue on $channel")
    consumer = channel.map{ ch =>
      val qc = new QueueingConsumer(ch)
      ch.basicConsume(queue.name, autoAck, consumerTag.tag, noLocal, exclusive, arguments, qc)
      qc
    }
  }
}
