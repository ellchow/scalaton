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
import scalaz.stream._, Process._
import scalaz.concurrent._
import scala.collection.immutable.TreeMap
import scalaton.util.monoids._

object Join {
  /* cogroup 2 processes - ASSUMES both are sorted by K */
  def coGroup[L, R, K](lefts: Process[Task,(K,L)], rights: Process[Task,(K,R)])(implicit kord: Ordering[K]): Process[Task,(K, (Vector[L],Vector[R]))] = {
    val chunkedLeft: Process[Task,Option[(K, Vector[L])]] = (lefts.map(_.some) ++ emit(None))
      .chunkBy2(_.map(_._1) == _.map(_._1)).map{ xs =>
      val ys = xs.collect{ case Some(x) => x }
      ys.headOption.map(_._1 -> ys.map(_._2))
    }.collect{ case x@Some(_) => x } ++ constant(none)
    val chunkedRight: Process[Task,Option[(K, Vector[R])]] = (rights.map(_.some) ++ emit(None))
      .chunkBy2(_.map(_._1) == _.map(_._1)).map{ xs =>
      val ys = xs.collect{ case Some(x) => x }
      ys.headOption.map(_._1 -> ys.map(_._2))
    }.collect{ case x@Some(_) => x } ++ constant(none)

    sealed trait Action
    case object Left extends Action
    case object Right extends Action
    case object Both extends Action
    def go(prevL: Option[(K, Vector[L])], prevR: Option[(K, Vector[R])], action: Option[Action], maxL: Option[K], maxR: Option[K]):
        Process.Tee[Option[(K, Vector[L])], Option[(K, Vector[R])], (K, (Vector[L], Vector[R]))] =
      action match {
        case Some(Both) =>
          for {
            l <- awaitL[Option[(K, Vector[L])]]
            r <- awaitR[Option[(K, Vector[R])]]
            out <- {
              val checkL = (for { max <- maxL ; curr <- l } yield kord.lt(max, curr._1)).getOrElse(true)
              val checkR = (for { max <- maxR ; curr <- r } yield kord.lt(max, curr._1)).getOrElse(true)
              (checkL, checkR) match {
                case (true, true) => go(l, r, None, l.map(_._1.some).getOrElse(maxL), r.map(_._1.some).getOrElse(maxR))
                case (false, _) => Halt(new IllegalArgumentException("left is not sorted"))
                case (_, false) => Halt(new IllegalArgumentException("right is not sorted"))
              }
            }
          } yield out
        case Some(Left) =>
          for {
            l <- awaitL[Option[(K, Vector[L])]]
            out <- {
              val checkL = (for { max <- maxL ; curr <- l } yield kord.lt(max, curr._1)).getOrElse(true)
              checkL match {
                case true => go(l, prevR, None, l.map(_._1.some).getOrElse(maxL) , maxR)
                case false => Halt(new IllegalArgumentException("left is not sorted"))
              }
            }
          } yield out
        case Some(Right) =>
          for {
            r <- awaitR[Option[(K, Vector[R])]]
            out <- {
              val checkR = (for { max <- maxR ; curr <- r } yield kord.lt(max, curr._1)).getOrElse(true)
              checkR match {
                case true => go(prevL, r, None, maxL, r.map(_._1.some).getOrElse(maxR))
                case false => Halt(new IllegalArgumentException("right is not sorted"))
              }
            }
          } yield out
        case None =>
          (prevL, prevR) match {
            case (None, None) => halt
            case (Some((kl, ls)), Some((kr, rs))) =>
              if (kl < kr) {
                emit(kl -> (ls, Vector.empty)) ++ go(none, prevR, Left.some, maxL, maxR)
              } else if (kl > kr) {
                emit(kr -> (Vector.empty, rs)) ++ go(prevL, none, Right.some, maxL, maxR)
              } else {
                emit(kl -> (ls, rs)) ++ go(none, none, Both.some, maxL, maxR)
              }
            case (Some((kl, ls)), None) => emit(kl -> (ls, Vector.empty)) ++ go(none, none, Left.some, maxL, maxR)
            case (None, Some((kr, rs))) => emit(kr -> (Vector.empty, rs)) ++ go(none, none, Right.some, maxL, maxR)
          }
      }
    chunkedLeft.tee(chunkedRight)(go(none, none, Both.some, none, none))
  }

  def fullOuterJoin[L, R, K : Ordering](lefts: Process[Task,(K,L)], rights: Process[Task,(K,R)]): Process[Task,(K, (Option[L], Option[R]))] = {
    for {
      (k, (ls, rs)) <- coGroup(lefts, rights)
      ol <- if (ls.nonEmpty) emitAll(ls.map(_.some)) else emit(None)
      or <- if (rs.nonEmpty) emitAll(rs.map(_.some)) else emit(None)
    } yield (k, (ol, or))
  }

  def leftOuterJoin[L, R, K : Ordering](lefts: Process[Task,(K,L)], rights: Process[Task,(K,R)]): Process[Task,(K, (L, Option[R]))] =
    fullOuterJoin(lefts,rights).collect{ case x@(k, (Some(l), r)) => (k, (l, r)) }

  def rightOuterJoin[L, R, K : Ordering](lefts: Process[Task,(K,L)], rights: Process[Task,(K,R)]): Process[Task,(K, (Option[L],R))] =
    fullOuterJoin(lefts,rights).collect{ case x@(k, (l, Some(r))) => (k, (l, r)) }

  def innerJoin[L, R, K : Ordering](lefts: Process[Task,(K,L)], rights: Process[Task,(K,R)]): Process[Task,(K, (L,R))] =
    for {
      (k, (ls, rs)) <- coGroup(lefts, rights)
      l <- emitAll(ls)
      r <- emitAll(rs)
    } yield (k, (l, r))

}
