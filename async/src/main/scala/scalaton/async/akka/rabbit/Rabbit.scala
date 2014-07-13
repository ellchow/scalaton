package scalaton.async.akka.rabbit

import akka.actor._, SupervisorStrategy._
import com.rabbitmq.client._
import scala.collection.JavaConversions._
import scala.concurrent.{ Future, ExecutionContext }
import scala.concurrent.duration._
import scalaton.async.akka._
import scalaton.util._
import scalaton.util.Json._
import scalaz._, Scalaz._
import argonaut._, Argonaut._
import java.util.UUID
import scala.util.{ Try, Success, Failure }

object Manager {
  import Amqp._

  case object Connect
  case class DeclareExchange(
    exchange: Exchange,
    exchangeType: ExchangeType = ExchangeType.Direct,
    isDurable: Boolean = false,
    autoDelete: Boolean = true,
    isInternal: Boolean = false,
    arguments: Map[String,java.lang.Object] = Map.empty
  )

  case class DeclareQueue(
    exchange: Exchange,
    queue: Queue,
    routingKey: RoutingKey = RoutingKey(""),
    durable: Boolean = false,
    exclusive: Boolean = false,
    autoDelete: Boolean = true,
    arguments: Map[String,java.lang.Object] = Map.empty
  )
  case class BindQueue(exchange: Exchange, queue: Queue, routingKey: RoutingKey)

  case class GetPublisher(exchange: Exchange)
  case class NoSuchExchangeDeclared(exchange: Exchange)
  case class PublisherFor(actor: ActorRef, exchange: Exchange)

  private[rabbit] case object GetChannel
  private[rabbit] case class SetChannel (channel: Channel)

  // case class GetQueue(exchange: String, queue: String)
  // case class Queue(actorRef: ActorRef)

  sealed trait State
  case class Connected(since: Long = System.currentTimeMillis) extends State
  case class Disconnected(since: Long = System.currentTimeMillis) extends State
  case object Idle extends State

}

class Manager(connP: Amqp.ConnectionParams,
  reconnectRetryInterval: FiniteDuration = 500.millis,
  maxDisconnectedDuration: FiniteDuration = 20.seconds,
  publisherExecutionContext: ExecutionContext = ExecutionContext.Implicits.global,
  consumerExecutionContext: ExecutionContext = ExecutionContext.Implicits.global) extends Actor with ActorLogging {

  import Amqp._
  import Manager._

  val connF = connP.factory

  private var exchanges: Map[Exchange,DeclareExchange] = Map.empty // exchange name -> exchange declaration
  private var queues: Map[(Exchange,Queue),DeclareQueue] = Map.empty // (exchange name, queue name) -> queue declaration
  private var publishers: Map[Exchange,ActorRef] = Map.empty // exchange name -> publisher actor
  private var listeners: Map[(Exchange,Queue),ActorRef] = Map.empty // (exchange name, queue name) -> listener actor


  private var connCh: Option[(Connection, Channel)] = None
  private var state: State = Idle

  // receive
  lazy val receive: Receive = idled

  lazy val onError: Receive = {
    case e: ShutdownSignalException =>
      log.warning(s"connection lost due to: ${e} - reconnecting")
      context.become(disconnected)
      reconnect(reconnectRetryInterval)
  }
  def withErrorHandler(r: Receive) = r.orElse(onError)

  def idled: Receive = {
    log.info(s"rabbit connection ${connP.uri()} is idle")
    state = Idle

    withErrorHandler({ case Connect => connect() })
  }

  def connected: Receive = {
    log.info(s"connected to ${connCh}")
    state match {
      case Connected(_) =>
      case _ =>
        state = Connected()
    }

    withErrorHandler({
      case de@DeclareExchange(exchange, exchangeType, isDurable, autoDelete, isInternal, arguments) =>
        connCh match {
          case Some((_, channel)) =>
            log.info(s"declaring exchange $exchange")
            exchanges = exchanges + (exchange -> de)
            log.debug(s"""exchanges (${exchanges.size}): ${exchanges.keys.toSeq.sorted.mkString(",")}""")
            channel.exchangeDeclare(exchange.name, exchangeType.name, isDurable, autoDelete, isInternal, arguments)
            makePublisher(exchange)

          case None =>
            log.error(s"no valid rabbit connection")
        }

      case dq@DeclareQueue(exchange, queue, routingKey, durable, exclusive, autoDelete, arguments) =>
        connCh match {
          case Some((_, channel)) =>
            log.info(s"declaring queue $queue on exchange $exchange")
            queues = queues + ((exchange, queue) -> dq)
            channel.queueDeclare(queue.name, durable, exclusive, autoDelete, arguments)
            self ! BindQueue(exchange, queue, routingKey)
          case None =>
            log.error(s"no valid rabbit connection")
        }

      case bq@BindQueue(exchange, queue, routingKey) =>
        connCh match {
          case Some((_, channel)) =>
            log.info(s"binding queue $queue on exchange $exchange with routing key $routingKey")
            channel.queueBind(queue.name, exchange.name, routingKey.id)
          case None =>
            log.error(s"no valid rabbit connection")
        }

      case GetPublisher(exchange) =>
        log.debug(s"request for publisher on $exchange from ${sender.path.name}")
        publishers.get(exchange) match {
          case Some(a) => sender ! PublisherFor(a, exchange)
          case None => sender ! NoSuchExchangeDeclared(exchange)
        }

      case GetChannel =>
        println((sender.path.name, connCh))
        connCh.foreach{ case (_, ch) => sender ! SetChannel(ch) }

    })
  }

  def disconnected: Receive = {
    state match {
      case Disconnected(_) =>
      case _ =>
        state = Disconnected()
    }
    connCh = None

    withErrorHandler({ case Connect => connect() })
  }

  override def postStop(): Unit = {
    close()
  }

  def close() = {
    log.info("closing rabbit connections")
    connCh.foreach{ case (connection, channel) =>
      try {
        channel.close()
        connection.close()
      } catch {
        case _: Throwable =>
      }
    }
    context.become(idled)
  }

  def reconnect(t: FiniteDuration) = state match {
    case Connected(_) =>
    case Disconnected(since) if (System.currentTimeMillis - since).millis > maxDisconnectedDuration =>
      log.warning("exceeded maximum disconnected duration of ${maxDisconnectedDuration}")
      close()
    case _ =>
      if (state != Idle) log.info(s"reconnecting in $t")
      context.system.scheduler.scheduleOnce(t, self, Connect)(context.dispatcher)
  }


  def connect(): Unit = {
    try {
      connCh = connCh.map{ x =>
        log.debug(s"already connected to ${connCh}")
        x.some
      }.getOrElse{
        log.info(s"connecting to ${connP.uri()}")

        val connection = connF.newConnection

        connection.addShutdownListener(new ShutdownListener {
          def shutdownCompleted(cause: ShutdownSignalException) = {
            self ! cause
          }
        })

        val channel = connection.createChannel
        (publishers.values ++ listeners.values).foreach(_ ! SetChannel(channel))

        (connection, channel).some
      }



      context.become(connected)
    } catch {
      case _: java.net.ConnectException =>
        reconnect(reconnectRetryInterval)
    }
  }

  // actor children

  def makePublisher(exchange: Exchange): Unit = publishers.get(exchange) match {
    case Some(_) =>
    case None =>
      connCh.foreach{ case (_, channel) =>
        val a = context.actorOf(Props(new Publisher(exchange, channel)(publisherExecutionContext)))
        context.watch(a)

        publishers = publishers + (exchange -> a)
      }
  }

}

object Publisher {
  import Amqp._

  case class Publish[A : EncodeJson](msg: A, routingKey: RoutingKey, mandatory: Boolean = false, immediate: Boolean = false, props: AMQP.BasicProperties = null) {
    def json = msg.asJson
  }
  case class PublishOk(pub: Publish[_])
  case class PublishFailed(pub: Publish[_])
}
class Publisher(exchange: Amqp.Exchange, private var channel: Channel)(implicit executionContext: ExecutionContext) extends Actor with ActorLogging {
  import Publisher._
  import Manager._

  private var counter = 0L // used for request id generation
  private var requests: Map[Long, (Publish[_], ActorRef)] = Map.empty // request id -> (publish message, requester)

  lazy val receive: Receive = {
    case SetChannel(ch) =>
      channel = ch

    case p@Publish(msg, routingKey, mandatory, immediate, props) =>
      val id = nextId()
      requests = requests + (id -> (p, sender))

      context.actorOf(Props(new FutureRetrier(id,{
        channel.basicPublish(exchange.name, routingKey.id, mandatory, immediate, props, p.json.toString.getBytes)
      }, List.fill(3)(1.seconds))))

    case Retrier.Result(id, Success(_)) =>
      requests.get(id) match {
        case Some((p, a)) =>
          a ! PublishOk(p)
          requests = requests - id
        case None =>
      }

    case Retrier.Result(id, Failure(e)) =>
      requests.get(id) match {
        case Some((p, a)) =>
          a ! PublishFailed(p)
          requests = requests - id
        case None =>
      }
  }

  protected def nextId() = {
    val id = counter
    counter += 1
    id
  }
}


object Listener {
  import Amqp._

  case class Delivery[A : DecodeJson] private[rabbit] (delivery: QueueingConsumer.Delivery) {
    lazy val message = new String(delivery.getBody).decodeEither[A]
    def tag = delivery.getEnvelope.getDeliveryTag
    def exchange = delivery.getEnvelope.getExchange
    def routingKey = delivery.getEnvelope.getRoutingKey
    def isRedeliver = delivery.getEnvelope.isRedeliver
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
    noLocal: Boolean = false
  )(f: Delivery[A] => ProcessStatus[A])(implicit dec: DecodeJson[A], ec: ExecutionContext) =
    Props(new Listener[A](rabbitManager, queue, consumerTag, autoAck, exclusive, arguments, noLocal){
      def processDelivery(d: Delivery[A]) = f(d) }
    )
}

abstract class Listener[A : DecodeJson](
  rabbitManager: ActorRef,
  queue: Amqp.Queue,
  consumerTag: Amqp.ConsumerTag = Amqp.ConsumerTag(UUID.randomUUID.toString),
  autoAck: Boolean = false,
  exclusive: Boolean = false,
  arguments: Map[String,java.lang.Object] = Map.empty,
  noLocal: Boolean = false)(implicit ec: ExecutionContext) extends Actor with ActorLogging {
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

    case Failure(e: ShutdownSignalException) =>
      log.error(e.stackTrace)
      context.become(idle)

    case Failure(e) =>
      log.error(e.stackTrace)
      awaitDelivery()

  }: Receive)

  def processDelivery(d: Delivery[A]): ProcessStatus[A]

  def awaitDelivery(): Unit =
    consumer.foreach{ qc =>
      // log.debug(s"awaiting next delivery")
      Future(Delivery[A](qc.nextDelivery))(ec).onComplete{ res => self ! res }
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

/*
object Main extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  import Amqp._
  import scalaton.util.Json._

  val system = ActorSystem("rabbit")
  val manager = system.actorOf(Props(new Manager(Amqp.ConnectionParams())))
  manager ! Manager.Connect

  val exch = Exchange("exch")
  val qq = Queue("qq")
  val rk = RoutingKey("xxx")

  system.actorOf(Props(new Actor with ActorLogging {

    manager ! Manager.DeclareExchange(exch)
    manager ! Manager.DeclareQueue(exch, qq, rk)
    context.system.scheduler.scheduleOnce(1.second, manager, Manager.GetPublisher(exch))

    lazy val consumer = context.actorOf(Listener.props[String](manager, qq){ d =>
      println(d.message)
      Listener.Ack(d)
    })

    val receive: Receive = {
      case Manager.PublisherFor(pub, _) =>
        context.become(processing(pub))
    }

    def processing(pub: ActorRef): Receive = {
      log.debug(s"begin publishing")
      consumer

      Future((1 to 20).foreach{ i => Thread.sleep(1000) ; pub ! Publisher.Publish(s"    hello world $i", rk) })

      { case x => log.info(x.toString) }
    }

  }))

  akka.pattern.after(30.seconds, system.scheduler)(Future{
    println("shutting down system...")
    system.shutdown
  })
}
*/
