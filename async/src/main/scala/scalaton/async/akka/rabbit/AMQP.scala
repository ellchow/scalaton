package scalaton.async.akka.rabbit

object Amqp {
  case class Ack()
  case class Nack()

  case class Exchange(val name: String) extends AnyVal
  case class Queue(val name: String) extends AnyVal
  case class RoutingKey(val key: String) extends AnyVal

  sealed trait ExchangeType {
    def name: String
  }
  object ExchangeType {
    case object Direct extends ExchangeType { val name = "direct" }
  }




}
