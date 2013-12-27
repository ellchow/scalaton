/*
 Copyright 2013 Elliot Chow

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

package scalaton.collection.immutable

import org.scalacheck._
import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._
import scala.collection.immutable.Queue
import scala.util.{ Try, Success, Failure }

class BankersQueueSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  behavior of "banker's queue"

  sealed trait QueueOp[+A]
  case class Enqueue[+A](elem: A) extends QueueOp[A]
  case object Dequeue extends QueueOp[Nothing]

  def exec[A](q: BankersQueue[A], op: QueueOp[A]) = op match {
    case Enqueue(elem) => ((), q.enqueue(elem))
    case Dequeue => q.dequeue
  }

  def exec[A](q: Queue[A], op: QueueOp[A]) = op match {
    case Enqueue(elem) => ((), q.enqueue(elem))
    case Dequeue => q.dequeue
  }

  it should "behave the same as scala.collection.immutable.Queue" in {
    lazy val genQueueOps: Gen[List[QueueOp[Int]]] = for {
      xs <- Arbitrary.arbitrary[List[Int]]
      ops = xs.map(x => Enqueue(x)) ++ List.fill(xs.size)(Dequeue)
    } yield scala.util.Random.shuffle(ops)

    implicit val arbitraryQueueOps = Arbitrary(genQueueOps)

    forAll {
      (queueOps: List[QueueOp[Int]]) => {
        var bq: (Any, BankersQueue[Int]) = ((), BankersQueue.empty)
        var qq: (Any, Queue[Int]) = ((), Queue.empty)

        val actual = queueOps.map{ op =>
          val tryb = Try(exec(bq._2, op))
          val tryq = Try(exec(qq._2, op))

          (tryb, tryq) match {
            case (Success(b), Success(q)) =>
              if (b._1 == q._1 && b._2.toList == q._2.toList){
                bq = b
                qq = q
                0
              }else {
                1
              }
            case (Failure(eb: NoSuchElementException), Failure(eq: NoSuchElementException)) => 0
            case _ => 1
          }
        }

        actual.sum should be(0)
      }
    }

  }

}
