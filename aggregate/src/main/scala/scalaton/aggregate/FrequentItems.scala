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

package scalaton.aggregate

import scala.collection.immutable.TreeMap

import scalaton.util.monoids._

/* heavy hitters algo described in https://www.cs.ucsb.edu/research/tech_reports/reports/2005-23.pdf using less space-efficient data structure */
trait FrequentItemsModule {
  case class FrequentItems[A](val size: Int, val buckets: TreeMap[Long, Set[A]], val lookup: Map[A, (Long, Long)]) {
    def get(a: A) = lookup.get(a)

    def countOf(a: A): Option[Long] = get(a).map(_._1)

    def errorOf(a: A): Option[Long] = get(a).map(_._2)

    def increment(a: A, w: Long = 1L) = {
      require(w > 0)
      if(lookup.contains(a)){
        val count = countOf(a).get

        val b = buckets.get(count).map(_ - a).getOrElse(Set.empty)
        val updatedBuckets = (if(b.isEmpty) buckets - count else buckets.updated(count, b))
          .updated(count + 1, buckets.get(count + 1).getOrElse(Set.empty) + a)

        val updatedLookup = lookup.updated(a, lookup(a).copy(_1 = count + 1))

        this.copy(size, updatedBuckets, updatedLookup)
      }else{
        if(lookup.size < size){
          val updatedBuckets = buckets.updated(w, buckets.get(w).getOrElse(Set.empty) + a)

          val updatedLookup = lookup.updated(a, (w, 0L))

          this.copy(size, updatedBuckets, updatedLookup)
        }else{
          val (minCount, minAs) = buckets.head

          val updatedBuckets = buckets.drop(1) + ((minCount + 1) -> Set(a))
          val updatedLookup = (lookup -- minAs) + (a -> (minCount + w, minCount))

          this.copy(size, updatedBuckets, updatedLookup)
        }
      }
    }

    def top(k: Int) = buckets.takeRight(k).toList.reverse.flatMap(_._2).map{ a => (a, lookup(a)) }
  }

  object FrequentItems{
    def empty[A](size: Int) = FrequentItems[A](size, TreeMap.empty, Map.empty)

    def fromData[A](size: Int, as: Iterable[A]) = as.foldLeft(empty[A](size))((ss, a) => ss.increment(a))
  }
}
object spacesaving extends FrequentItemsModule

/*
import scalaton.aggregate.spacesaving._
def time(f: =>Unit) = { val t = System.currentTimeMillis; f; println((System.currentTimeMillis - t) / 1000.0 + " seconds")}

val as = (0 to 10000000).view.map(_ => 1 + math.sqrt(util.Random.nextInt(1000000)).toInt + (if(util.Random.nextBoolean) 1 else -1))
time({
val truth = as.groupBy(identity).map{ case (k, vs) => (k, vs.size) }.toSeq.sortBy(-_._2)
truth.take(20).foreach(println)
 })
time({
val x = FrequentItems.fromData(1000, as)
x.top(20).foreach(println)
 })

*/
