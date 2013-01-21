package scalaton.stats.singlepass

import scala.language.postfixOps

import scala.collection.BitSet

import scalaz.{BloomFilter => _, _}
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._

/**
 * Immutable bloom Filter implementation
 *
 * http://en.wikipedia.org/wiki/Bloom_filter
 */

object BloomFilter{

  def apply[A, B](numItems: Int, fpProb: Double,
                  seed: Long = 0L)(items: A*)
                                  (implicit h: Hashable[A, B],
                                   hconv: HashCodeConverter[B, Int]): BloomFilter[A,B] = {
    val (numHashes, width) = optimalParameters(numItems, fpProb)

    val z: BloomFilter[A,B] = BFZero(numHashes, width, seed)(h, hconv)

    items.foldLeft(z)((acc, x) => acc + x)
  }

  def optimalParameters(numItems: Int, fpProb: Double) = {
    val width = optimalWidth(numItems, fpProb)
    val numHashes = optimalNumHashes(numItems, width)

    (numItems, width)
  }

  /** http://en.wikipedia.org/wiki/Bloom_filter#Probability_of_false_positives **/
  def optimalNumHashes(numItems: Int, width: Int): Int =
    math.ceil(width / numItems * math.log(2)).toInt + 1

  /** http://en.wikipedia.org/wiki/Bloom_filter#Probability_of_false_positives **/
  def optimalWidth(numItems: Int, fpProb: Double): Int =
    math.ceil(-1 * numItems * math.log(fpProb) / math.log(2) / math.log(2)).toInt + 1

  def BloomFilterMonoid[A,B](parameters: (Int, Int), seed: Long = 0L)
                            (implicit h: Hashable[A, B],
                             hconv: HashCodeConverter[B, Int]): Monoid[BloomFilter[A,B]] with Equal[BloomFilter[A,B]] = {
    val (numHashes, width) = parameters
    new Monoid[BloomFilter[A,B]] with Equal[BloomFilter[A,B]]{

      def equal(bf1: BloomFilter[A,B], bf2: BloomFilter[A,B]) = {
        require(zero.hasSameParameters(bf2),
                "bloom filter equality must use Equal with same parameters")
        require(bf1.hasSameParameters(bf2),
                "bloom filters not comparable if parameters are different")

        Tags.Conjunction((bf1, bf2) match {
          case (x1: BFZero[A,B], x2: BFZero[A,B]) => true
          case (x1: BFInstance[A,B], x2: BFInstance[A,B]) => x1.bits == x2.bits
          case (x1: BFZero[A,B], x2: BFInstance[A,B]) => x2.bits.isEmpty
          case (x1: BFInstance[A,B], x2: BFZero[A,B]) => x1.bits.isEmpty
          case _ => false
        })
      }

      override val zero: BloomFilter[A,B] = BFZero[A,B](numHashes, width, seed)(h, hconv)

      def append(bf1: BloomFilter[A,B], bf2: => BloomFilter[A,B]): BloomFilter[A,B] = {
        require(bf1.hasSameParameters(bf2),
                "cannot combine bloom filters with different parameters")

        (bf1, bf2) match {
          case (x1: BFZero[A,B], x2: BFZero[A,B]) => zero
          case (x1: BFZero[A,B], x2: BFInstance[A,B]) => x2
          case (x1: BFInstance[A,B], x2: BFZero[A,B]) => x1
          case (x1: BFInstance[A,B], x2: BFInstance[A,B]) =>
            BFInstance(numHashes, width, x1.bits ++ x2.bits, seed)(h, hconv)
        }
      }
    }
  }
}

sealed trait BloomFilter[A,B]{

  /** Number of hashes in this bloom filter **/
  val numHashes: Int

  /** Number of bits in this bloom filter **/
  val width: Int

  /** Hash seed for this bloom filter **/
  val seed: Long

  /**
   * Check if parameters are equal - compatibility check
   * Would be nice to make this type safe instead
   **/
  def hasSameParameters(other: BloomFilter[A,B]) =
  (numHashes === other.numHashes) && (width === other.width) && (seed === other.seed)

  /** Compute hash of an item for this bloom filter **/
  def hashItem(item: A)(implicit h: Hashable[A, B],
                        hconv: HashCodeConverter[B, Int]): BitSet = {
    val hcs = (multiHash(item, seed)(h) |> hconv.convertSeq) take numHashes
    BitSet(hcs.toSeq map { _ % width |> HashCode } : _*)
  }

  /** Add item to this bloom filter **/
  def + (item: A): BloomFilter[A, B]

  /** Remove item (probabilistic) from this bloom filter **/
  def - (item: A): BloomFilter[A, B]

  /** Test for (probabilistic) existence of an item **/
  def contains(item: A): Boolean
}

/**
 * Representation of an empty bloom filter
 **/
case class BFZero[A,B](val numHashes: Int,
                       val width: Int,
                       val seed: Long = 0L)
                      (implicit h: Hashable[A, B],
                       hconv: HashCodeConverter[B, Int])
     extends BloomFilter[A, B]{

  def + (item: A): BloomFilter[A, B] =
    BFInstance(numHashes,width,hashItem(item),seed)(h,hconv)

  def - (item: A): BloomFilter[A, B] = this

  def contains(item: A): Boolean = false
}

/**
 * Representation of a (possibly) nonempty bloom filter
 **/
case class BFInstance[A, B](val numHashes: Int,
                            val width: Int,
                            val bits: BitSet,
                            val seed: Long = 0L
                          )
                           (implicit h: Hashable[A, B],
                            hconv: HashCodeConverter[B, Int])
     extends BloomFilter[A, B]{

  def + (item: A): BloomFilter[A, B] = construct(bits ++ hashItem(item))

  def - (item: A): BloomFilter[A, B] = construct(bits -- hashItem(item))

  def contains(item: A): Boolean = {
    val itemBits = hashItem(item)
                           (bits & itemBits) == itemBits
  }

  private def construct(b: BitSet) =
    BFInstance(numHashes, width, b, seed)(h, hconv)
}
