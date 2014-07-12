package scalaton.async.akka.rabbit


import akka.actor._, SupervisorStrategy._
import com.rabbitmq.client._
import scala.collection.JavaConversions._
import scala.concurrent.Future
import scala.concurrent.duration._
import scalaton.async.akka._
import scalaz._, Scalaz._
import argonaut._, Argonaut._

object AMQP {
  case class Ack()
  case class Nack()

  sealed trait ExchangeType {
    def name: String
  }
  object ExchangeType {
    case object Direct extends ExchangeType { val name = "direct" }
  }
}

object RabbitManager {
  import AMQP._

  case object Connect
  case class DeclareExchange(exchange: String, exchangeType: ExchangeType = ExchangeType.Direct, isDurable: Boolean = false, autoDelete: Boolean = true, isInternal: Boolean = true, arguments: Map[String,java.lang.Object] = Map.empty)
  case class DeclareQueue[A : EncodeJson : DecodeJson](exchange: String, queue: String, constructor: () => RabbitQueue[A], routingKey: String = "", durable: Boolean = false, exclusive: Boolean = false, autoDelete: Boolean = true, arguments: Map[String,java.lang.Object] = Map.empty)

  case class ExchangeDeclared(exchange: String)
  case class QueueDeclared(exchange: String, queue: String)

  sealed trait State
  case class Connected(since: Long = System.currentTimeMillis) extends State
  case class Disconnected(since: Long = System.currentTimeMillis) extends State
  case object Idle extends State

  def queueName(exchange: String, queue: String) = s"${exchange}+${queue}+queue"
}

class RabbitManager(connF: ConnectionFactory) extends Actor with ActorLogging {
  import AMQP._
  import RabbitManager._

  val reconnectRetryInterval = 500.millis
  val maxDisconnectedDuration = 5.seconds


  private var connCh: Option[(Connection, Channel)] = None
  private var exchanges: Map[String,DeclareExchange] = Map.empty // exchange name -> exchange declaration
  private var queues: Map[(String,String),DeclareQueue[_]] = Map.empty // (exchange name, queue name) -> queue declaration
  private var workers: Map[ActorRef, (String,String)] = Map.empty // rabbit worker -> (exchange name, queue name)
  private var state: State = Idle

  lazy val receive = ({
    case Connect => connect()

    case de@DeclareExchange(exchange, exchangeType, isDurable, autoDelete, isInternal, arguments) =>
      log.info(s"declaring exchange $exchange")
      connCh.foreach{ case (_, channel) =>
        exchanges = exchanges + (exchange -> de)
        log.debug(s"""exchanges (${exchanges.size}): ${exchanges.keys.toSeq.sorted.mkString(",")}""")
        channel.exchangeDeclare(exchange, exchangeType.name, isDurable, autoDelete, isInternal, arguments)

      }

    case dq@DeclareQueue(exchange, queue, constructor, routingKey, durable, exclusive, autoDelete, arguments) =>
      log.info(s"declaring queue $queue on exchange $exchange")
      connCh.foreach{ case (_, channel) =>
        queues = queues + ((exchange, queue) -> dq)
        val w = context.actorOf(Props(constructor()), queueName(exchange,queue))
        workers = workers + (w -> (exchange, queue))
        channel.queueDeclare(queue, durable, exclusive, autoDelete, arguments)
        channel.queueBind(queue, exchange, routingKey)
      }

  }: Receive).orElse(onError)

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

  lazy val onError: Receive = {
    case e: ShutdownSignalException =>
      log.warning(s"connection lost due to: ${e} - reconnecting")
      disconnected()
      reconnect(reconnectRetryInterval)
  }



  override def postStop(): Unit = {
    close()
  }

  protected def reconnect(t: FiniteDuration) = state match {
    case Connected(_) =>
    case Disconnected(since) if (System.currentTimeMillis - since).millis > maxDisconnectedDuration =>
      log.warning("exceeded maxDisconnectedDuration - idling")
      close()
    case _ =>
      if (state != Idle) log.info(s"reconnecting in $t")
      context.system.scheduler.scheduleOnce(t, self, Connect)(context.dispatcher)
  }


  protected def connect(): Unit = {
    try {
      connCh = connCh.map{ x =>
        log.debug(s"already connected to ${connCh}")
        x.some
      }.getOrElse{
        log.info(s"connecting to ${connF.toUri()}")

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
}

abstract class RabbitQueue[Msg : EncodeJson : DecodeJson] extends Actor with ActorLogging {
  def akkaReceive: Receive
  def processMessage(msg: Msg): Unit

  lazy val receive: Receive = rabbitReceive.orElse(akkaReceive)

  lazy val rabbitReceive: Receive = {
    case s: String => s.decodeEither[Msg] match {
      case \/-(msg) => processMessage(msg)
      case -\/(_) => akkaReceive(s)
    }
  }
}

object Main extends App {
  import scala.concurrent.ExecutionContext.Implicits.global

  val system = ActorSystem("rabbit")
  val manager = system.actorOf(Props(new RabbitManager(new ConnectionFactory)))
  manager ! RabbitManager.Connect

  akka.pattern.after(30.seconds, system.scheduler)(Future{
    println("shutting down system...")
    system.shutdown
  })

}
