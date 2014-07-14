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
  case class Declared(request: Any)

  case class GetPublisher(exchange: Exchange)
  case class NoSuchExchangeDeclared(exchange: Exchange)
  case class PublisherFor(actor: ActorRef, exchange: Exchange)


  private[rabbit] case object GetChannel
  private[rabbit] case class SetChannel (channel: Channel)


  sealed trait State
  case class Connected(since: Long = System.currentTimeMillis) extends State
  case class Disconnected(since: Long = System.currentTimeMillis) extends State
  case object Idle extends State

  case object GetState

}

class Manager(connP: Amqp.ConnectionParams,
  reconnectRetryInterval: FiniteDuration = 3.seconds,
  maxDisconnectedDuration: FiniteDuration = 30.seconds,
  publisherExecutionContext: ExecutionContext = ExecutionContext.Implicits.global,
  consumerExecutionContext: ExecutionContext = ExecutionContext.Implicits.global,
  autoConnect: Boolean = true
) extends Actor with ActorLogging {

  import Amqp._
  import Manager._

  def connF = connP.factory

  private var exchanges: Map[Exchange,(ActorRef, DeclareExchange)] = Map.empty // exchange name -> exchange declaration
  private var queues: Map[(Exchange,Queue),(ActorRef, DeclareQueue)] = Map.empty // (exchange name, queue name) -> queue declaration
  private var publishers: Map[Exchange,(ActorRef, Channel)] = Map.empty // exchange name -> publisher actor
  private var listeners: Map[ActorRef,Channel] = Map.empty // listener actor -> channel


  private var conn: Option[Connection] = None
  private var state: State = Idle

  override def preStart(): Unit = {
    if (autoConnect) context.system.scheduler.scheduleOnce(50.millis, self, Connect)(context.dispatcher)
  }

  // receive
  lazy val receive: Receive = idled

  lazy val commonReceive: Receive = {
    case GetState => sender ! state
  }

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
    conn = None

    withErrorHandler({ case Connect => connect() }).orElse(commonReceive)
  }

  def getOrElseCreateChannel(a: ActorRef) = {
    listeners.get(a) match {
      case l@Some(_) => l
      case None =>
        log.info(s"creating channel for ${a.path.name}")
        conn.foreach{ c =>
          listeners = listeners + (a -> c.createChannel)
        }
        listeners.get(a)
    }
  }


  def connected: Receive = {
    log.info(s"connected to ${conn}")
    state match {
      case Connected(_) =>
      case _ =>
        state = Connected()
    }

    def bindQ(channel: Channel, exchange: Exchange, queue: Queue, routingKey: RoutingKey) = {
      log.info(s"binding queue ${queue.name} to exchange ${exchange.name} with routing key ${routingKey.id} on ${channel}")
      channel.queueBind(queue.name, exchange.name, routingKey.id)
    }

    withErrorHandler({
      case de@DeclareExchange(exchange, exchangeType, isDurable, autoDelete, isInternal, arguments) =>
        getOrElseCreateChannel(sender) match {
          case Some(channel) =>
            log.info(s"declaring exchange $exchange on $channel")
            exchanges = exchanges + (exchange -> (sender, de))
            // log.debug(s"""exchanges (${exchanges.size}): ${exchanges.keys.toSeq.sorted.mkString(",")}""")
            channel.exchangeDeclare(exchange.name, exchangeType.name, isDurable, autoDelete, isInternal, arguments)
            makePublisher(exchange)
            sender ! Declared(de)

          case None =>
            log.error(s"no valid rabbit connection")
        }

      case dq@DeclareQueue(exchange, queue, routingKey, durable, exclusive, autoDelete, arguments) =>
        getOrElseCreateChannel(sender) match {
          case Some(channel) =>
            log.info(s"declaring queue $queue on exchange $exchange on ${channel}")
            queues = queues + ((exchange, queue) -> (sender, dq))
            channel.queueDeclare(queue.name, durable, exclusive, autoDelete, arguments)
            bindQ(channel, exchange, queue, routingKey)
            sender ! Declared(dq)
          case None =>
            log.error(s"no valid rabbit connection")
        }

      case bq@BindQueue(exchange, queue, routingKey) =>
        getOrElseCreateChannel(sender) match {
          case Some(channel) =>
            log.info(s"binding queue $queue on exchange $exchange with routing key $routingKey on ${channel}")
            bindQ(channel, exchange, queue, routingKey)
            sender ! Declared(bq)
          case None =>
            log.error(s"no valid rabbit connection")
        }

      case GetPublisher(exchange) =>
        log.debug(s"request for publisher on $exchange from ${sender.path.name}")
        publishers.get(exchange) match {
          case Some((a, _)) => sender ! PublisherFor(a, exchange)
          case None => sender ! NoSuchExchangeDeclared(exchange)
        }

      case GetChannel =>
        getOrElseCreateChannel(sender)
        context.watch(sender)
        listeners.get(sender).foreach(ch => sender ! SetChannel(ch))

      case Terminated(a) =>
        listeners.get(a) match {
          case Some(ch) =>
            try { ch.close } catch { case _: Throwable => }
            listeners = listeners - a
          case None =>
        }
        publishers.values.find(_._1 == a) match {
          case Some((_, ch)) =>
            try { ch.close } catch { case _: Throwable => }
            publishers = publishers.filter(_._2._1 != a)
          case None =>
        }

    }).orElse(commonReceive)
  }

  def disconnected: Receive = {
    state match {
      case Disconnected(_) =>
      case _ =>
        state = Disconnected()
    }
    conn = None

    withErrorHandler({ case Connect => connect() }).orElse(commonReceive)
  }

  override def postStop(): Unit = {
    close()
  }

  def close() = {
    log.info("closing rabbit channels and connection")
    listeners.foreach{ case (_, ch) =>
      try { ch.close() }
      catch { case _: Throwable => }
    }
    publishers.foreach{ case (_, (_, ch)) =>
      try { ch.close() }
      catch { case _: Throwable => }
    }
    conn.foreach{ c =>
      try { c.close() }
      catch { case _: Throwable => }
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
      conn = conn.map{ c =>
        log.debug(s"already connected to ${c}")
        c.some
      }.getOrElse{
        log.info(s"connecting to ${connP.uri()}")

        val connection = connF.newConnection

        connection.addShutdownListener(new ShutdownListener {
          def shutdownCompleted(cause: ShutdownSignalException) = {
            self ! cause
          }
        })

        // recreate all pre-existing channels
        publishers = publishers.map{ case (ex, (a, _)) => (ex, (a, connection.createChannel)) }
        listeners = listeners.map{ case (a, _) => (a, connection.createChannel) }
        // re-declare exchanges and queues - note: independent bindings are currently not tracked
        (exchanges.values ++ queues.values).foreach{ case (a, decl) => self.tell(decl, a) }
        // notify subscribers of new channels to be used
        (publishers.map(_._2) ++ listeners).foreach{ case (a, ch) => context.system.scheduler.scheduleOnce(250.millis, a, SetChannel(ch))(context.dispatcher) }

        connection.some
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
      conn.foreach{ c =>
        val channel = c.createChannel
        val a = context.actorOf(Props(new Publisher(exchange, channel)(publisherExecutionContext)), s"publisher+${exchange.name}")
        context.watch(a)
        publishers = publishers + (exchange -> (a, channel))
      }
  }

}

/*
object Main extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  import Amqp._
  import scalaton.util.Json._

  val system = ActorSystem("rabbit")
  val manager = system.actorOf(Props(new Manager(Amqp.ConnectionParams())), "manager")

  val exch = Exchange("exch")
  val qq = Queue("qq")
  val rk = RoutingKey("xxx")
  val consumerTag = ConsumerTag()

  system.actorOf(Props(new Actor with ActorLogging {
    // should wait properly for connection setup
    context.system.scheduler.scheduleOnce(750.millis, manager, Manager.DeclareExchange(exch))
    context.system.scheduler.scheduleOnce(1000.millis, manager, Manager.DeclareQueue(exch, qq, rk))
    // should wait properly for declarations to be done
    context.system.scheduler.scheduleOnce(1500.millis, manager, Manager.GetPublisher(exch))

    lazy val consumer = context.actorOf(Listener.props[String](manager, qq, consumerTag = consumerTag){ d =>
      println(("start", d.message))
      // Thread.sleep(3000)
      // println(("end", d.message))
      Listener.Ack(d)
    }, s"${qq.name}+${consumerTag.tag}")

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

  }), "main")

  akka.pattern.after(30.seconds, system.scheduler)(Future{
    println("shutting down system...")
    system.shutdown
  })
}
 */
