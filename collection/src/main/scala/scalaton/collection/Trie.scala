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

import scala.collection._
import scala.collection.generic._
import scala.collection.mutable

class Trie[A, +B] private[immutable] (val value: Option[B], val suffixes: Map[A, Trie[A,B]]) extends Map[List[A], B] with MapLike[List[A], B, Trie[A,B]] {

  override def empty = new Trie(None, Map.empty)

  def +[B1 >: B](kv: (List[A], B1)): Trie[A,B1] = kv._1 match {
    case Nil => new Trie(Some(kv._2), suffixes)

    case prefix :: suffix if suffixes.contains(prefix) =>
      new Trie(value, suffixes.updated(prefix, suffixes(prefix) + (suffix -> kv._2)))

    case prefix :: suffix =>
      new Trie(value, suffixes + (prefix -> Trie.fromList(suffix, kv._2)))
  }

  def -(key: List[A]) = key match {
    case Nil => this

    case prefix :: rest =>
      if(suffixes.contains(prefix))
        new Trie(value, suffixes.updated(prefix, suffixes(prefix) - rest))
      else
        this
  }

  def get(key: List[A]) = key match {
    case Nil => value
    case prefix :: rest if suffixes.contains(prefix) => suffixes(prefix).get(rest)
    case _ => None
  }

  def iterator = ((for {
    (prefix, t) <- suffixes
    (suffix, value) <- t
    _ = (suffix, value) // FIXME: removing this leads to stack overflow??
  } yield (prefix :: suffix, value)) ++ value.map(v => Map(Nil -> v)).getOrElse(Map.empty)).iterator

  def withPrefix(prefix: List[A]): Trie[A,B] = prefix match {
    case Nil => this
    case a :: as => suffixes.get(a).map(_.withPrefix(as)).getOrElse(empty)
  }

  def removePrefix(prefix: List[A]): Trie[A,B] = prefix match {
    case Nil => empty

    case a :: as if suffixes.contains(a) => new Trie(value, suffixes.updated(a, suffixes(a).removePrefix(as)))

    case _ => this
  }

}

object Trie {
  def empty[A,B] = new Trie(None, Map.empty[A,Trie[A,B]])

  def apply[A,B](kvs: (List[A], B)*): Trie[A,B] =
    kvs.foldLeft(empty[A,B])((t, kv) => t + kv)

  def newBuilder[A,B]: mutable.Builder[(List[A], B), Trie[A,B]] =
    new mutable.MapBuilder[List[A], B, Trie[A,B]](empty[A,B])

  implicit def canBuildFrom[A,B]
      : CanBuildFrom[Trie[_,_], (List[A], B), Trie[A,B]] =
    new CanBuildFrom[Trie[_,_], (List[A], B), Trie[A,B]] {
      def apply(from: Trie[_,_]) = newBuilder[A,B]
      def apply() = newBuilder[A,B]
    }

  def fromList[A,B](as: List[A], v: B): Trie[A,B] = as match {
    case Nil => new Trie(Some(v), Map.empty)
    case a :: rest => new Trie(None, Map(a -> fromList(rest, v)))
  }
}
