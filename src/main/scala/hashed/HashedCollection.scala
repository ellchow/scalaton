package scalaton.hashed

import scala.language.higherKinds
import scala.language.postfixOps

import scala.collection.{SortedSet, BitSet}

import scalaz.{BloomFilter => _, _}
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._


/**
 * Type classes used for defining different hashed collections
 */

/** Characterizes a collection that will hash items (consistently) **/
trait Hashes[A,B,C]{

  /** Number of hashes used in this collection **/
  val numHashes: Int

  /** Seed of hashing function **/
  val seed: Long

  /** Compute hash of an item **/
  def hashItem(item: A)(implicit h: Hashable[A, B],
                        hconv: HashCodeConverter[B, C]): Iterable[C @@ HashCode] = {
    (multiHash(item, seed)(h) |> hconv.convertSeq take numHashes)
  }

}


/** Collections in which you can insert items **/
trait Inserts[A,B,C,F] extends Hashes[A,B,C]{
  def insert(collection: F, item: A)(implicit h: Hashable[A, B],
                                     hconv: HashCodeConverter[B, C]): F
}

/** Collections in which you can instantiate with a single item **/
trait MakesSingleton[A,B,C,F] extends Inserts[A,B,C,F] with Monoid[F] {
  def singleton(item: A)(implicit h: Hashable[A, B],
                         hconv: HashCodeConverter[B, C]): F
}

/** Collections in which you can check for existence of an item **/
trait Contains[A,B,C,F] extends Hashes[A,B,C]{
  def contains(collection: F, item: A)(implicit h: Hashable[A, B],
                                       hconv: HashCodeConverter[B, C]): Boolean
}

/** Collections in which you can check the size/cardinality **/
trait Sizes[F]{
  def cardinality(collection: F): Long
}



/**
 * Functions for operating on hashed collections
 **/

trait InsertsFunctions{
  def insert[A,B,C,F](collection: F, item: A)(implicit i: Inserts[A,B,C,F],
                                               h: Hashable[A, B],
                                               hconv: HashCodeConverter[B, C]) =
    i.insert(collection, item)
}

trait MakesSingletonFunctions{
  def singleton[A,B,C,F](item: A)(implicit ms: MakesSingleton[A,B,C,F],
                                  h: Hashable[A, B],
                                  hconv: HashCodeConverter[B, C]): F =
    ms.singleton(item)
}


trait ContainsFunctions{
  def contains[A,B,C,F](collection: F, item: A)(implicit c: Contains[A,B,C,F],
                                                h: Hashable[A, B],
                                                hconv: HashCodeConverter[B, C]) =
    c.contains(collection, item)
}

trait SizesFunctions{
  def cardinality[F](collection: F)(implicit s: Sizes[F]): Long =
    s.cardinality(collection)
}


