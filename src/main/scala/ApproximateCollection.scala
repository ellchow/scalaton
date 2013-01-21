package scalaton

import scala.language.postfixOps

import scala.collection.BitSet

import scalaz.{BloomFilter => _, _}
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._

trait ApproximateCollection[A]

trait HashedCollection[A,B,C] extends ApproximateCollection[A]{
  /** Number of hashes used in this collection **/
  val numHashes: Int

  /** Seed of hashing function **/
  val seed: Long

  /** Compute hash of an item **/
  def hashItem(item: A)(implicit h: Hashable[A, B],
                        hconv: HashCodeConverter[B, C]): Iterable[C @@ HashCode] =
    multiHash(item, seed)(h) |> hconv.convertSeq take numHashes

}


