package scalaton.async.akka.rabbit

object Amqp {
  case class Ack()
  case class Nack()

  case class Exchange(val name: String) extends AnyVal
  case class Queue(val name: String) extends AnyVal
  case class RoutingKey(val id: String) extends AnyVal

  sealed trait ExchangeType {
    def name: String
  }
  object ExchangeType {
    case object Direct extends ExchangeType { val name = "direct" }
  }


  implicit val exchangeOrdering = new Ordering[Exchange] { def compare(x: Exchange, y: Exchange) = implicitly[Ordering[String]].compare(x.name,y.name) }
  implicit val queueOrdering = new Ordering[Queue] { def compare(x: Queue, y: Queue) = implicitly[Ordering[String]].compare(x.name,y.name) }
  implicit val routingKeyOrdering = new Ordering[RoutingKey] { def compare(x: RoutingKey, y: RoutingKey) = implicitly[Ordering[String]].compare(x.id,y.id) }

}
