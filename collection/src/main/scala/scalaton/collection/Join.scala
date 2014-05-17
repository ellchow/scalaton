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

import scalaz.{ Ordering => _ , _ }, Scalaz._
import scalaz.stream._
import scalaz.concurrent._
import scala.collection.immutable.TreeMap
import scalaton.util.monoids._

object Join {
  /* cogroup 2 processes, assume both are sorted by K */
  def coGroup[L, R, K : Ordering](lefts: Process[Task,(K,L)], rights: Process[Task,(K,R)]) = {
    val mixed = scalaz.stream.merge.mergeN(Process(lefts.map(_.left), rights.map(_.right)))

    case class Info(
      val lk: Option[K] = None,
      val lm: TreeMap[K, Vector[L]] = TreeMap.empty,
      val rk: Option[K] = None,
      val rm: TreeMap[K, Vector[R]] = TreeMap.empty)

    implicit val infoSemigroup = new Semigroup[Info] {
      def append(i1: Info, i2: =>Info) = Info(
        i2.lk.map(_.some).getOrElse(i1.lk),
        i1.lm |+| i2.lm,
        i2.rk.map(_.some).getOrElse(i1.rk),
        i1.rm |+| i2.rm
      )
    }

    val kord = implicitly[Ordering[K]]

    mixed.scan((Info(), Vector.empty[(K,(Option[Vector[L]],Option[Vector[R]]))])){
      case ((info@Info(None, _, _, _), _), -\/((k, l))) =>
        (info |+| Info(lk = k.some, lm = TreeMap(k -> Vector(l))), Vector.empty)

      case ((info@Info(_, _, None, _), _), \/-((k, r))) =>
        (info |+| Info(rk = k.some, rm = TreeMap(k -> Vector(r))), Vector.empty)

      case ((info@Info(Some(lk), lm, _, _), _), -\/((k, l))) =>
        if (lk == k) {
          (info |+| Info(lm = TreeMap(k -> Vector(l))), Vector.empty)
        } else {
          if (info.rk.map(_ > lk).getOrElse(false)) {
            (info.copy(lk = k.some, lm = info.lm - lk, rm = info.rm.dropWhile(x => !kord.gt(x._1,lk))),
              info.rm.takeWhile(x => kord.lt(x._1, lk))
                .map{ case (k,vs) => (k, (None, Some(vs))) }.toVector  :+ (lk, (info.lm.get(lk), info.rm.get(lk)))
            )
          } else {
            (info |+| Info(lk = k.some, lm = TreeMap(k -> Vector(l))), Vector.empty)
          }
        }

      case ((info@Info(_, _, Some(rk), rm), _), \/-((k, r))) =>
        if (rk == k) {
          (info |+| Info(rm = TreeMap(k -> Vector(r))), Vector.empty)
        } else {
          if (info.lk.map(_ > rk).getOrElse(false)) {
            (info.copy(lm = info.lm.dropWhile(x => !kord.gt(x._1,rk)), rk = k.some, rm = info.rm - rk),
              info.lm.takeWhile(x => kord.lt(x._1, rk))
                .map{ case (k,vs) => (k, (Some(vs), None)) }.toVector :+ ((rk, (info.lm.get(rk), info.rm.get(rk))))
            )
          } else {
            (info |+| Info(rk = k.some, rm = TreeMap(k -> Vector(r))), Vector.empty)
          }
        }
    }
  }
}

/*
object Join {

  implicit def iterable2iterator[A](i: Iterable[A]) = i.iterator

  abstract class OuterJoin[K : Ordering, A] {

    def fullOuterJoin[B](right: Iterator[(K, B)]): Iterator[(K,(Option[A],Option[B]))]

    def rightOuterJoin[B](right: Iterator[(K, B)]): Iterator[(K,(Option[A],B))] =
      fullOuterJoin(right).collect{ case (k, (oa, Some(b))) => (k, (oa, b)) }

    def leftOuterJoin[B](right: Iterator[(K, B)]): Iterator[(K,(A,Option[B]))] =
      fullOuterJoin(right).collect{ case (k, (Some(a), ob)) => (k, (a, ob)) }
  }

  /** joins on iterators, where both left and right inputs must be sorted */
  implicit class OrderedIteratorOrderedIteratorJoin[K : Ordering, A](left: Iterator[(K, A)]) extends OuterJoin[K,A] {

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

    def groupByKey =  new Iterator[(K, Seq[A])] {
      private val bufferedLeft = left.buffered

      private var prevKA: Option[K] = None

      private var nextAs = readConsecutiveKeys(bufferedLeft)

      def hasNext = nextAs._1.nonEmpty

      def next = {
        val (kaOption, as) = nextAs

        prevKA.zip(kaOption).foreach{ case (k1, k2) => require(!implicitly[Ordering[K]].gt(k1, k2), s"left keys are not ordered ($k1, $k2)") }
        prevKA = kaOption

        kaOption match {
          case Some(ka) =>
            nextAs = readConsecutiveKeys(bufferedLeft)

            (ka, as)

          case None => throw new NoSuchElementException("next on empty iterator")
        }
      }
    }

    def coGroup[B](right: Iterator[(K, B)]): Iterator[(K,(Seq[A],Seq[B]))] = {
      val groupedL = left.groupByKey.buffered
      val groupedR = right.groupByKey.buffered

      new Iterator[(K,(Seq[A],Seq[B]))] {
        def hasNext = groupedL.hasNext || groupedR.hasNext

        def next = {
          val ol = if (groupedL.nonEmpty) Some(groupedL.head) else None
          val or = if (groupedR.nonEmpty) Some(groupedR.head) else None

          (ol, or) match {
            case (Some((kl, l)), Some((kr, r))) =>
              val ord = implicitly[Ordering[K]]
              ord.compare(kl, kr) match {
                case 0 =>
                  groupedL.next()
                  groupedR.next()
                  (kl, (l, r))

                case cmp if cmp < 0 =>
                  groupedL.next()
                  (kl, (l, Vector.empty))

                case _ =>
                  groupedR.next()
                  (kr, (Vector.empty, r))
              }

            case (Some((kl, l)), None) =>
              groupedL.next()
              (kl, (l, Vector.empty))

            case (None, Some((kr, r))) =>
              groupedR.next()
              (kr, (Vector.empty, r))

            case _ => throw new NoSuchElementException("next called on empty iterator")
          }
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
  }

  implicit def iterableToIteratorJoin[K : Ordering,A](i: Iterable[(K, A)]) = new OrderedIteratorOrderedIteratorJoin(i.iterator)

  /** hash join implementation */
  implicit class MapSingleIteratorJoin[K : Ordering, A](left: Map[K, A]) extends OuterJoin[K,A] {
    def innerJoin[B](right: Iterator[(K, B)]): Iterator[(K,(A,B))] = for {
      (k, b) <- right
      a <- left.get(k)
    } yield (k, (a, b))

    /** requires right to be sorted */
    def fullOuterJoin[B](right: Iterator[(K, B)]) =
      left.toSeq.sortBy(_._1).iterator.fullOuterJoin(right)

  }

}
 */
