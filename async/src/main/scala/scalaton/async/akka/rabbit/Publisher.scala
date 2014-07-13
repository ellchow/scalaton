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
