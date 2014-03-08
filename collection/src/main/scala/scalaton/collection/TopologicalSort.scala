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

import scalaz._, Scalaz._

class TopologicalSort[A] private (val independents: Set[A], val dependencyMap: Map[A,Set[A]]) extends Iterable[A] { self =>
  if(independents.isEmpty && dependencyMap.nonEmpty) // prevent immediate construction of invalid graph (has cycle)
    throw new IllegalStateException("unreachable elements in topological sort graph")

  def +(a: A) = if (dependencyMap.contains(a)) this else new TopologicalSort(independents + a, dependencyMap)

  def +(dep: (A, A)): TopologicalSort[A] = dep match {
    case (before, after) if before != after =>
      new TopologicalSort(
        independents - after ++ (if (hasDependencies(before)) Set.empty else Set(before)), // make sure after is not independent, add before as independent if it does not already have dependencies
        dependencyMap |+| Map(after -> Set(before)) // update dependencyMap with this relation
      )
    case _ => this
  }

  def ++(deps: Iterable[(A, A)]) =
    deps.foldLeft(this){ case (t, (before, after)) => t + (before -> after) }

  def addDependents(dep: (A, Set[A])): TopologicalSort[A] = {
    (dep match {
      case (before, afters) => this ++ afters.map(after => (before, after))
    }) + dep._1
  }

  def addDependencies(dep: (Set[A], A)): TopologicalSort[A] = {
    (dep match {
      case (befores, after) => this ++ befores.map(before => (before, after))
    }) + dep._2
  }

  def -- (as: Set[A]) = {
    val init = (independents -- as, Map.empty[A,Set[A]]) // remove as from independents, create dependencyMap from scratch
    val (newIndependents, newDependencyMap) = dependencyMap.foldLeft(init){
      case ((is, map), (after, befores0)) =>
        if (as.contains(after)) {  // remove as that are afters in dependencyMap
          (is, map)
        } else {
          val befores = befores0 -- as // remove as that are befores in dependencyMap
          if (befores.isEmpty) {
            (is + after, map) // free up this after as it no longer has dependencies
          } else {
            (is, map + (after -> befores)) // add this after back, without removed dependencies
          }
        }
    }
    new TopologicalSort(newIndependents, newDependencyMap)
  }

  def - (a: A) = this -- Set(a)

  def pop: (Set[A], TopologicalSort[A]) = {
    val t = this -- independents
    (independents, t)
  }

  def contains(a: A) = independents.contains(a) || dependencyMap.contains(a)

  def iterator: Iterator[A] = new Iterator[A] {
    private var t = self
    private var is = Vector.empty[A]

    def hasNext = is.nonEmpty || t.nonEmpty

    def next = {
      if (is.nonEmpty) {
        val i = is.head
        is = is.drop(1)
        i
      } else {
        val nxt = t.pop
        is = nxt._1.toVector
        t = nxt._2
        next
      }
    }
  }

  override def isEmpty = independents.isEmpty && dependencyMap.isEmpty
  override def nonEmpty = !isEmpty

  def dependenciesOf(a: A) = dependencyMap.getOrElse(a, Set.empty)
  def hasDependencies(a: A) = dependenciesOf(a).nonEmpty

  def dependentsOf(a: A) = dependencyMap.flatMap{ case (after, befores) => if (befores.contains(a)) Some(after) else None }.toSet
  def hasDependents(a: A) = dependentsOf(a).nonEmpty

  override def toString = s"TopologicalSort($independents, $dependencyMap)"
}

object TopologicalSort {
  def empty[A]: TopologicalSort[A] = new TopologicalSort[A](Set.empty, Map.empty)

  def apply[A](dependencies: Iterable[(A,A)]): TopologicalSort[A] =
    empty[A] ++ dependencies

  def apply[A](dependencies: (A, A)*): TopologicalSort[A] =
    apply(dependencies)

  def independents[A](as: Iterable[A]): TopologicalSort[A] =
    as.foldLeft(empty[A])((t, a) => t + a)

  def independents[A](as: A*): TopologicalSort[A] =
    independents(as)
}
