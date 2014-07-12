package scalaton.async.akka

import com.rabbitmq.client._

package object rabbit {
  implicit class ConnectionFactoryOps(cf: ConnectionFactory) {
    def toUri(hidePassword: Boolean = true): String = {
      val pw = if (hidePassword) "******" else cf.getPassword
      s"amqp://${cf.getUsername}:${pw}@${cf.getHost}:${cf.getPort}/${cf.getVirtualHost}"
    }
  }
}
