package scalaton.async.akka.rabbit

import com.rabbitmq.client._
import scala.concurrent.duration._

object Amqp {
  case class Exchange(val name: String) extends AnyVal
  case class Queue(val name: String) extends AnyVal
  case class RoutingKey(val id: String) extends AnyVal
  case class ConsumerTag(val tag: String) extends AnyVal

  sealed trait ExchangeType {
    def name: String
  }
  object ExchangeType {
    case object Direct extends ExchangeType { val name = "direct" }
    case object Fanout extends ExchangeType { val name = "fanout" }
  }

  case class ConnectionParams(
    host: String = ConnectionFactory.DEFAULT_HOST,
    port: Int = ConnectionFactory.USE_DEFAULT_PORT,
    vhost: String = ConnectionFactory.DEFAULT_VHOST,
    username: String = ConnectionFactory.DEFAULT_USER,
    password: String = ConnectionFactory.DEFAULT_PASS,
    timeout: FiniteDuration = 10.seconds,
    heartbeat: FiniteDuration = ConnectionFactory.DEFAULT_HEARTBEAT.seconds,
    // networkRecoveryInterval: FiniteDuration = 5000.millis,
    // topologyRecovery: Boolean = true,
    // autoRecovery: Boolean = true,
    // amqpSslPort: Int = ConnectionFactory.DEFAULT_AMQP_OVER_SSL_PORT,
    // amqpPort: Int = ConnectionFactory.DEFAULT_AMQP_PORT,
    channelMax: Int = ConnectionFactory.DEFAULT_CHANNEL_MAX,
    frameMax: Int = ConnectionFactory.DEFAULT_FRAME_MAX

  ) {
    lazy val factory = {
      val f = new ConnectionFactory
      f.setHost(host)
      f.setPort(port)
      f.setVirtualHost(vhost)
      f.setUsername(username)
      f.setPassword(password)
      f.setConnectionTimeout(timeout.toMillis.toInt)
      f.setRequestedHeartbeat(heartbeat.toSeconds.toInt)
      f.setRequestedChannelMax(channelMax)
      f.setRequestedFrameMax(frameMax)
      // f.setNetworkRecoveryInterval(networkRecoveryInterval.toMillis.toInt)
      f.setTopologyRecoveryEnabled(true)
      f.setAutomaticRecoveryEnabled(false)
      f
    }

    def uri(hidePassword: Boolean = true) = factory.toUri(hidePassword)
  }


  implicit val exchangeOrdering = new Ordering[Exchange] { def compare(x: Exchange, y: Exchange) = implicitly[Ordering[String]].compare(x.name,y.name) }
  implicit val queueOrdering = new Ordering[Queue] { def compare(x: Queue, y: Queue) = implicitly[Ordering[String]].compare(x.name,y.name) }
  implicit val routingKeyOrdering = new Ordering[RoutingKey] { def compare(x: RoutingKey, y: RoutingKey) = implicitly[Ordering[String]].compare(x.id,y.id) }

}
