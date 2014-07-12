package scalaton.async.akka.rabbit

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
