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

package scalaton.util

import scalaz._
import Scalaz._

trait SearchModule{

  // search for element in sorted list - if it does not exist return the index at which to insert it
  def binarySearch[A : math.Ordering](xs: collection.IndexedSeq[A], a: A): Either[Int,Int] = {
    val ord = implicitly[math.Ordering[A]]

    sealed trait Direction
    case object Up extends Direction
    case object Down extends Direction

    @annotation.tailrec
    def loop(lb: Int, ub: Int, dir: Direction): Either[Int,Int] = {
      lazy val midpoint = (ub - lb) / 2 + lb

      if(ub lt lb){
        dir match {
          case Up => Left(lb)
          case Down => Left(ub + 1)
        }
      }else if(ord.equiv(xs(midpoint), a)){
        Right(midpoint)
      }else if(ord.lt(a, xs(midpoint))){
        loop(lb, midpoint - 1, Down)
      }else{
        loop(midpoint + 1, ub, Up)
      }
    }

    loop(0, xs.size - 1, Up)
  }
}


object search extends SearchModule


// import scalaz._, Scalaz._
// import scalaton.util.search._

// val zs = for(_ <- 0 until 10000) yield {
//   val xs = (0 to 1000).map(_ => util.Random.nextInt(200)).sorted.distinct

//   val i = util.Random.nextInt(400)

//   // (scalaton.util.search.binarySearch(xs, i).getOrElse(-1) === xs.indexOf(i)) ? 1 | 0

//   (scalaton.util.search.binarySearch(xs, i).getOrElse(-1) gt -1) ? 1 | 0
// }

// zs.sum
