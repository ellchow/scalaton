package scalaton

import scala.language.postfixOps

import scala.collection.BitSet

import scalaz.{BloomFilter => _, _}
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._

/**
 * Bloom filter trait
 *
 * http://en.wikipedia.org/wiki/Bloom_filter
 */

trait BloomFilter[A,B]{

  /** Number of hashes in this bloom filter **/
  val numHashes: Int

  /** Number of bits/cells in this bloom filter **/
  val width: Int

  /** Hash seed for this bloom filter **/
  val seed: Long

  require(numHashes gt 0, "number of hashes needs to be > 0")
  require(width gt 0, "width needs to be > 0")

  /** Compute hash of an item for this bloom filter **/
  def hashItem(item: A)(implicit h: Hashable[A, B],
                        hconv: HashCodeConverter[B, Int]): Seq[Int @@ HashCode] = {
    val hcs = (multiHash(item, seed)(h) |> hconv.convertSeq) take numHashes
    hcs.toSeq map { _ % width |> HashCode }
  }

  /** Add item to this bloom filter **/
  def + (item: A): BloomFilter[A, B]

  /** Test for (probabilistic) existence of an item **/
  def contains(item: A): Boolean
}


