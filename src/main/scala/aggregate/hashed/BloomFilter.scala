package scalaton.aggregate.hashed

import scala.collection.BitSet

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._


/**
 * Bloom filter
 *
 * http://en.wikipedia.org/wiki/Bloom_filter
 */
trait BloomFilter[A,B,F]
extends HashedCollection[A,B,Int,F]
with MakesSingleton[A,B,Int,F]
with SetLike[A,B,Int,F]
with Sized[F] {

  val width: Int

  /** could possibly use double hashing **/
  override def hashItem(item: A)(implicit h: Hashable[A, B],
                                 hconv: HashCodeConverter[B, Int]): Iterable[Int @@ HashCode] = {
    // val hcs = super.hashItem(item)(h,hconv) take 2 toSeq
    // (0 until numHashes) map { i => HashCode(math.abs(hcs(0) + i * hcs(1) + i * i).toInt % width) }
    super.hashItem(item) map { _ % width |> HashCode}
  }
}


object bloomfilter
extends StandardBloomFilterInstances
with HashedCollectionFunctions
with MakesSingletonFunctions
with SetLikeFunctions
with MapLikeFunctions
with SizedFunctions
