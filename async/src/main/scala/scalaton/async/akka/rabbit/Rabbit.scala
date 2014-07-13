package scalaton.async.akka.rabbit

import akka.actor._, SupervisorStrategy._
import com.rabbitmq.client._
import scala.collection.JavaConversions._
import scala.concurrent.{ Future, ExecutionContext }
import scala.concurrent.duration._
import scalaton.async.akka._
import scalaton.util.Json._
import scalaz._, Scalaz._
import argonaut._, Argonaut._
import java.util.UUID

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

  // case class GetQueue(exchange: String, queue: String)
  // case class Queue(actorRef: ActorRef)

  sealed trait State
  case class Connected(since: Long = System.currentTimeMillis) extends State
  case class Disconnected(since: Long = System.currentTimeMillis) extends State
  case object Idle extends State

}

class Manager(connP: Amqp.ConnectionParams,
  publisherExecutionContext: ExecutionContext = ExecutionContext.Implicits.global,
  consumerExecutionContext: ExecutionContext = ExecutionContext.Implicits.global) extends Actor with ActorLogging {

  import Amqp._
  import Manager._

  val reconnectRetryInterval = 500.millis
  val maxDisconnectedDuration = 5.seconds

  val connF = connP.factory
  private var connCh: Option[(Connection, Channel)] = None
  private var exchanges: Map[Exchange,DeclareExchange] = Map.empty // exchange name -> exchange declaration
  private var queues: Map[(Exchange,Queue),DeclareQueue] = Map.empty // (exchange name, queue name) -> queue declaration
  private var state: State = Idle
  private var publishers: Map[Exchange,ActorRef] = Map.empty // exchange name -> publisher actor
  private var listeners: Map[(Exchange,Queue),ActorRef] = Map.empty // (exchange name, queue name) -> listener actor

  // receive

  lazy val receive = ({
    case Connect => connect()

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
      publishers.get(exchange) match {
        case Some(a) => sender ! PublisherFor(a, exchange)
        case None => sender ! NoSuchExchangeDeclared(exchange)
      }

  }: Receive).orElse(onError)

  lazy val onError: Receive = {
    case e: ShutdownSignalException =>
      log.warning(s"connection lost due to: ${e} - reconnecting")
      disconnected()
      reconnect(reconnectRetryInterval)
  }

  override def postStop(): Unit = {
    close()
  }

  // set states

  def connected() = {
    state match {
      case Connected(_) =>
      case _ =>
        state = Connected()
    }
  }

  def disconnected() = {
    state = Disconnected()
    connCh = None
  }

  def idle() = {
    state = Idle
    connCh = None
  }

  // connection handling

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
    idle()
  }

  def reconnect(t: FiniteDuration) = state match {
    case Connected(_) =>
    case Disconnected(since) if (System.currentTimeMillis - since).millis > maxDisconnectedDuration =>
      log.warning("exceeded maxDisconnectedDuration - idling")
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
          (connection, channel).some
      }

      connected()

      log.debug(s"connected to ${connCh}")
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

  case class SetChannel private (channel: Channel)
  case class Publish(msg: WEJson, routingKey: RoutingKey, mandatory: Boolean = false, immediate: Boolean = false, props: AMQP.BasicProperties)
  case class PublishOk(pub: Publish)
  case class PublishFailed(pub: Publish)
}
class Publisher(exchange: Amqp.Exchange, private var channel: Channel)(implicit executionContext: ExecutionContext) extends Actor with ActorLogging {
  import Publisher._

  private var requests: Map[Publish, ActorRef] = Map.empty

  lazy val receive: Receive = {
    case SetChannel(ch) =>
      channel = ch

    case p@Publish(msg, routingKey, mandatory, immediate, props) =>
      context.actorOf(Props(new FutureRetrier(123,{
        channel.basicPublish(exchange.name, routingKey.id, mandatory, immediate, props, msg.asJson.toString.getBytes)
        p
      }, List.fill(3)(1.seconds))))

    case Success(p: Publish) =>
      requests.get(p) match {
        case Some(a) =>
          a ! PublishOk(p)
          requests = requests - p
        case None =>
      }

    case Failure(p: Publish) =>
      requests.get(p) match {
        case Some(a) =>
          a ! PublishFailed(p)
          requests = requests - p
        case None =>
      }
  }
}


object Listener {
  case class SetChannel(channel: Channel)
}

abstract class Listener(implicit ec: ExecutionContext) extends Actor with ActorLogging {
  import Amqp._
  import Manager._
  import Listener._

  val rabbitManager: ActorRef
  val queue: Queue
  val routingKey: RoutingKey
  val consumerTag: ConsumerTag = ConsumerTag(UUID.randomUUID.toString)

  val autoAck = false
  val exclusive = false
  val arguments = Map.empty[String,java.lang.Object]
  val noLocal = false


  private var channel: Option[Channel] = None
  private var consumer: Option[QueueingConsumer] = None

  override def preStart(): Unit = {
    context.system.scheduler.scheduleOnce(500.millis, rabbitManager, GetChannel)(context.dispatcher)
  }

  lazy val receive: Receive = {
    case SetChannel(ch) =>
      log.info(s"setting rabbit channel to $ch")
      channel = ch.some
      makeConsumer()
  }


  def makeConsumer(): Unit = {
    consumer match {
      case Some(_) =>
      case None =>
        consumer = channel.map{ ch =>
          val qc = new QueueingConsumer(ch)
          ch.basicConsume(queue.name, autoAck, consumerTag.tag, noLocal, exclusive, arguments, qc)
          qc
        }
    }
  }

  // lazy val rabbitReceive: Receive = {
  //   case s: String => s.decodeEither[Msg] match {
  //     case \/-(msg) => processMessage(msg)
  //     case -\/(_) => akkaReceive(s)
  //   }
  // }
}





object Main extends App {
  import scala.concurrent.ExecutionContext.Implicits.global

  val system = ActorSystem("rabbit")
  val manager = system.actorOf(Props(new Manager(Amqp.ConnectionParams())))
  manager ! Manager.Connect

  akka.pattern.after(30.seconds, system.scheduler)(Future{
    println("shutting down system...")
    system.shutdown
  })

}
