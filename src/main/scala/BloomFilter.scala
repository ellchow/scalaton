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

trait BloomFilter[A,B] extends HashedCollection[A,B,Int]{

  /** Number of bits/cells in this bloom filter **/
  val width: Int

  override def hashItem(item: A)(implicit h: Hashable[A, B],
                        hconv: HashCodeConverter[B, Int]): Iterable[Int @@ HashCode] =
    super.hashItem(item)(h,hconv) map { _ % width |> HashCode }


  require(numHashes gt 0, "number of hashes needs to be > 0")
  require(width gt 0, "width needs to be > 0")

  /** Add item to this bloom filter **/
  def + (item: A): BloomFilter[A, B]

  /** Test for (probabilistic) existence of an item **/
  def contains(item: A): Boolean
}


