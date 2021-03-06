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

import scala.collection.immutable.TreeMap

import scalaz._
import Scalaz._

import shapeless._

trait MonoidInstances{
  implicit val maxIntMonoid: Monoid[Int @@ Tags.Max] = Monoid instance ((l, r) => Tag(l max r), Tags.Max(Int.MinValue))
  implicit val minIntMonoid: Monoid[Int @@ Tags.Min] = Monoid instance ((l, r) => Tag(l min r), Tags.Min(Int.MaxValue))

  implicit val maxDoubleMonoid: Monoid[Double @@ Tags.Max] = Monoid instance ((l, r) => Tag(l max r), Tags.Max(Double.MinValue))
  implicit val minDoubleMonoid: Monoid[Double @@ Tags.Min] = Monoid instance ((l, r) => Tag(l min r), Tags.Min(Double.MaxValue))

  implicit val maxLongMonoid: Monoid[Long @@ Tags.Max] = Monoid instance ((l, r) => Tag(l max r), Tags.Max(Long.MinValue))
  implicit val minLongMonoid: Monoid[Long @@ Tags.Min] = Monoid instance ((l, r) => Tag(l min r), Tags.Min(Long.MaxValue))

  implicit val maxShortMonoid: Monoid[Short @@ Tags.Max] = Monoid instance ((l, r) => Tag(l max r), Tags.Max(Short.MinValue))
  implicit val minShortMonoid: Monoid[Short @@ Tags.Min] = Monoid instance ((l, r) => Tag(l min r), Tags.Min(Short.MaxValue))

  implicit val maxByteMonoid: Monoid[Byte @@ Tags.Max] = Monoid instance ((l, r) => Tag(l max r), Tags.Max(Byte.MinValue))
  implicit val minByteMonoid: Monoid[Byte @@ Tags.Min] = Monoid instance ((l, r) => Tag(l min r), Tags.Min(Byte.MaxValue))


  implicit val hnilMonoid: Monoid[HNil] = Monoid instance((l, r) => HNil, HNil)
  implicit def hlistMonoid[H : Monoid, T <: HList : Monoid]: Monoid[H :: T] = {
    val hMonoid = implicitly[Monoid[H]]
    val tMonoid = implicitly[Monoid[T]]

    Monoid instance((l, r) => hMonoid.append(l.head, r.head) :: tMonoid.append(l.tail, r.tail), hMonoid.zero :: tMonoid.zero)
  }

  implicit def treeMapMonoid[K : scala.math.Ordering,V: Semigroup]: Monoid[TreeMap[K,V]] =
    Monoid instance ((l,r) => implicitly[Monoid[Map[K,V]]].append(l: Map[K,V], r: Map[K,V]).asInstanceOf[TreeMap[K,V]],
                     TreeMap.empty: TreeMap[K,V])


  /*
  implicit def mapTaggedMonoid[K, V, T](implicit semigroupVT: Semigroup[V @@ T]): Monoid[Map[K, V] @@ T] = new Monoid[Map[K, V] @@ T] {
    def zero: Map[K, V] @@ T  = Tag(Map[K, V]())

    def append(m1: Map[K, V] @@ T, m2: => Map[K, V] @@ T) = {
      // Eagerly consume m2 as the value is used more than once.
      val m2Instance: Map[K, V] = m2
      // semigroups are not commutative, so order may matter.
      val (from, to, semigroup) = {
        if (m1.size > m2Instance.size) (m2Instance, m1, (a: V, b: V) => semigroupVT.append(Tag(a), Tag(b)))
        else (m1, m2Instance, ((a: V, b: V) => semigroupVT.append(Tag(a), Tag(b))).flip)
      }

      val z = from.foldLeft(to) {
        case (to, (k, v)) => to + (k -> to.get(k).map(semigroup(_, v)).getOrElse(v))
      }

      Tag(z)
    }
  }
  */
}

object monoids
extends MonoidInstances
