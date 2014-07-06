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
  def coGroup[L, R, K : Ordering](lefts: Process[Task,(K,L)], rights: Process[Task,(K,R)]): Process[Task,(K, (Vector[L],Vector[R]))] = {
    val chunkedLeft = (lefts.map(_.some) ++ emit(None))
      .chunkBy2(_.map(_._1) == _.map(_._1))
      .map(_.collect{ case Some(x) => x }.left[Vector[(K,R)]].some)
      .append(emit(None))
    val chunkedRight = (rights.map(_.some) ++ emit(None))
      .chunkBy2(_.map(_._1) == _.map(_._1))
      .map(_.collect{ case Some(x) => x }.right[Vector[(K,L)]].some)
      .append(emit(None))

    def disjunctionToTuple[A,B](d: Vector[A] \/ Vector[B]): (Option[Vector[A]], Option[Vector[B]]) =
      d.fold(a => if (a.nonEmpty) (a.some, none) else (none, none), b => if (b.nonEmpty) (none, b.some) else (none, none))
    println("***************")
    println(lefts)
    println(rights)

    chunkedLeft.interleave(chunkedRight).scan(((none[Vector[(K,L)]], none[Vector[(K,R)]]), Seq.empty[(K, (Vector[L],Vector[R]))])){ case ((prev, _), c) =>
      println("----------")
      println((prev, c))
        val res = (prev, c) match {
        case ((Some(l), Some(r)), (Some(curr))) =>
          val kl = l.head._1
          val kr = r.head._1
          val out = if (kl == kr) {
            Seq(kl -> (l.map(_._2), r.map(_._2)))
          } else {
            Seq(
              kl -> (l.map(_._2), Vector.empty),
              kr -> (Vector.empty, r.map(_._2))
            ).sortBy(_._1)
          }

          (disjunctionToTuple(curr), out)

          case ((Some(l), None), Some(curr)) if curr.isRight =>
            ((disjunctionToTuple(curr)) |+| prev, Seq.empty)
          case ((Some(l), None), Some(curr)) =>
            ((disjunctionToTuple(curr)._1, none), Seq(l.head._1 -> (l.map(_._2), Vector.empty)))
          case ((None, Some(r)), Some(curr)) if curr.isLeft =>
            ((disjunctionToTuple(curr)) |+| prev, Seq.empty)
          case ((None, Some(r)), Some(curr)) =>
            ((none, disjunctionToTuple(curr)._2), Seq(r.head._1 -> (Vector.empty, r.map(_._2))))

          case ((None, None), Some(curr)) =>
            ((disjunctionToTuple(curr)) |+| prev, Seq.empty)

          case ((ol, or), None) =>
            val out = (ol, or) match {
              case (Some(l), Some(r))=>
                val kl = l.head._1
                val kr = r.head._1
                if (kl == kr) {
                  Seq(kl -> (l.map(_._2), r.map(_._2)))
                } else {
                  Seq(
                    kl -> (l.map(_._2), Vector.empty),
                    kr -> (Vector.empty, r.map(_._2))
                  ).sortBy(_._1)
                }
              case _ =>
                (ol.toSeq.map(x => x.head._1 -> (x.map(_._2), Vector.empty)) ++
                  or.toSeq.map(x => x.head._1 -> (Vector.empty, x.map(_._2)))).sortBy(_._1)
            }

            ((none, none), out)

      }

      println(("!!!",res))
      res
    }.flatMap{ case (_, out) =>
        println((">>>", out))
        emitAll(out)
    }.map{ i => println((">>><<<", i)); i }
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
