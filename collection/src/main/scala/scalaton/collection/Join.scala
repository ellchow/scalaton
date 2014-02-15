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

package scalaton.collection

import java.io._
import scala.language.implicitConversions

object Join {
  implicit class MapIteratorJoin[K : Ordering, A](left: Map[K, Seq[A]]) {
    def innerJoin[B](right: Iterator[(K, B)]): Iterator[(K,(A,B))] = for {
      (k, b) <- right
      a <- left.get(k).getOrElse(Seq.empty)
    } yield (k, (a, b))
  }

  implicit class MapSingleIteratorJoin[K : Ordering, A](left1: Map[K, A]) {
    private val left = left1.map{ case (k, v) => (k, Seq(v)) }

    def innerJoin[B](right: Iterator[(K, B)]): Iterator[(K,(A,B))] =
      left.innerJoin(right)
  }

  implicit class OrderedIteratorOrderedIteratorJoin[K : Ordering, A](left: Iterator[(K, A)]) {

    private def readConsecutiveKeys[T](bf: BufferedIterator[(K, T)]): (Option[K], Seq[T]) = {
      if(bf.nonEmpty) {
        val h = bf.head

        @annotation.tailrec
        def loop(ts: Vector[T]): Vector[T] = {
          if (bf.hasNext) {
            val (k, t) = bf.head
            if (h._1 == k) {
              bf.next
              loop(ts :+ t)
            } else
              ts
          } else {
            ts
          }
        }

        (Some(h._1), loop(Vector.empty))
      } else {
        (None, Vector.empty)
      }
    }

    def coGroup[B](right: Iterator[(K, B)]): Iterator[(K,(Seq[A],Seq[B]))] =
      new Iterator[(K,(Seq[A],Seq[B]))] {
        private val bufferedLeft = left.buffered
        private val bufferedRight = right.buffered

        private var nextAs = readConsecutiveKeys(bufferedLeft)
        private var nextBs = readConsecutiveKeys(bufferedRight)

        def hasNext = nextAs._1.nonEmpty || nextBs._1.nonEmpty

        def next = {
          val (kaOption, as) = nextAs
          val (kbOption, bs) = nextBs

          (kaOption, kbOption) match {
            case (Some(ka), Some(kb)) =>
              val ord = implicitly[Ordering[K]]
              ord.compare(ka, kb) match {
                case 0 =>
                  val nxt = (ka, (as, bs))
                  nextAs = readConsecutiveKeys(bufferedLeft)
                  nextBs = readConsecutiveKeys(bufferedRight)
                  nxt

                case x if x < 0 =>
                  val nxt = (ka, (as, Vector.empty))
                  nextAs = readConsecutiveKeys(bufferedLeft)
                  nxt

                case x if x > 0 =>
                  val nxt = (kb, (Vector.empty, bs))
                  nextBs = readConsecutiveKeys(bufferedRight)
                  nxt

                case _ => throw new Exception(s"${ord.toString}.compare is not returning valid values")
              }

            case (Some(ka), None) =>
              val nxt = (ka, (as, Vector.empty))
              nextAs = readConsecutiveKeys(bufferedLeft)
              nxt

            case (None, Some(kb)) =>
              val nxt = (kb, (Vector.empty, bs))
              nextBs = readConsecutiveKeys(bufferedRight)
              nxt

            case _ => throw new Exception("unreachable case")
          }
        }
      }

    def innerJoin[B](right: Iterator[(K, B)]): Iterator[(K,(A,B))] =
      for {
        (k, (as, bs)) <- left.coGroup(right)
        a <- as
        b <- bs
      } yield (k, (a, b))

    def fullOuterJoin[B](right: Iterator[(K, B)]): Iterator[(K,(Option[A],Option[B]))] =
      for {
        (k, (as, bs)) <- left.coGroup(right)
        oa <- if (as.nonEmpty) as.map(a => Some(a)) else Seq(None)
        ob <- if (bs.nonEmpty) bs.map(b => Some(b)) else Seq(None)
      } yield (k, (oa, ob))

    def rightOuterJoin[B](right: Iterator[(K, B)]): Iterator[(K,(Option[A],B))] =
      fullOuterJoin(right).collect{ case (k, (oa, Some(b))) => (k, (oa, b)) }

  }
}
