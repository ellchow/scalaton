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

package scalaton

package object aggregate {
  def dyadicDecomposition(lb: Int, ub: Int, n: Int = Int.MaxValue - 1): List[(Int,Int)] = {
    require(n <= (Int.MaxValue - 1))
    require(lb > 0)
    require(ub <= n)


    def wholeIntervals(lb0: Int, ub0: Int, level: Int): (List[(Int,Int)], List[(Int,Int)]) = {
      // convert to 0 based, upper bound exclusive
      val lb = lb0 - 1
      val ub = ub0

      // size of interval at this level
      val intervalSize = math.pow(2, level).toInt

      // get (potential) max upper and min lower bounds for intervals at this level
      val wholeLevelLb = intervalSize * math.ceil(lb.toDouble / intervalSize).toInt
      val wholeLevelUb = intervalSize * math.floor(ub.toDouble / intervalSize).toInt

      // there exists integer i s.t. [i*2^level, (i+1)*2^level] in [lb0, ub0]
      if(wholeLevelLb < wholeLevelUb){
        val numWholeIntervals = (wholeLevelUb - wholeLevelLb) / intervalSize
        ((0 until numWholeIntervals).toList map (i => (i * intervalSize + wholeLevelLb + 1,
          (i + 1) * intervalSize + wholeLevelLb)),
          List((lb0, wholeLevelLb), (wholeLevelUb + 1, ub0)))
      }else
        (List.empty, List((lb0, ub0)))
    }

    // not tailrec - depth bounded by log2(n)
    def loop(lb: Int, ub: Int, level: Int): List[(Int,Int)] = {
      if(lb > ub){
        Nil
      }else{
        wholeIntervals(lb, ub, level) match {
          case (intervals, recurse) =>
            intervals ++ recurse.flatMap{ case (l, u) => loop(l, u, level - 1) }
        }
      }
    }

    loop(lb, ub, math.ceil(math.log(n) / math.log(2)).toInt - 1)
  }
}
