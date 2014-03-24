/*
 Copyright 2014 Elliot Chow

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

package scalaton.async.akka

import akka.actor._
import scala.concurrent._, duration._
import scalaz._, Scalaz._

object IterActor {
  case class Object[A](val id: Long, val value: A)
  case object Next
  case object Done
}
class IterActor[A](getIterator: =>Iterator[A], subscribers: Set[ActorRef], batchSize: Int = 1) extends Actor with ActorLogging {
  import IterActor._

  lazy val iterator = getIterator
  var active = subscribers

  subscribers.foreach(s => context.watch(s))

  val receive: Receive = {
    self ! Next
    if (iterator.isEmpty) done else emitting(Set.empty)
  }

  def isCompleted = active.isEmpty || iterator.isEmpty

  @annotation.tailrec
  private def take(n: Int): Unit = if (n <= 0 || iterator.isEmpty) {
    Unit
  } else {
    val nxt = iterator.next
    active.foreach(_ ! nxt)
    take(n - 1)
  }

  def emitting(outstanding: Set[ActorRef]): Receive = {
    case Terminated(a) if subscribers.contains(a) =>
      log.debug("{} left", a.path.name)
      active = active - a
      context.become(emitting(outstanding - a))
      self ! Next

    case Next =>
      log.debug("received Next from {}", sender.path.name)
      val outstanding1 = outstanding - sender

      if (outstanding1.isEmpty) {
        take(batchSize)

        if (isCompleted)
          context.become(done)
        else
          context.become(emitting(active))
      } else {
        context.become(emitting(outstanding1))
      }
  }

  val done: Receive = {
    case _ => sender ! Done
  }
}

/*
import scalaton.async.akka._
import akka.actor._
import IterActor._

val system = ActorSystem("system")

val d = system.actorOf(Props(new Actor { val receive: Receive = { case i: Int => println("got " + i) ; case Done => println("done!") } }), "b")
val b = system.actorOf(Props(new Actor { val receive: Receive = { case i: Int => println("got " + i) ; case Done => println("done!") } }), "b")
val c = system.actorOf(Props(new Actor { val receive: Receive = { case i: Int => println("got " + i) ; case Done => println("done!") } }), "c")
val a = system.actorOf(Props(new IterActor((1 to 10).iterator, Set(b,c), 2)), "a")
a.tell(Next,b)
a.tell(Next,c)
b ! PoisonPill
a.tell(Next,c)
c ! PoisonPill
a.tell(Next,d)
*/
