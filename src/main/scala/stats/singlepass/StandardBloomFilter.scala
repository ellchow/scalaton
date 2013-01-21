package scalaton.stats.singlepass

import scala.language.postfixOps

import scala.collection.BitSet

import scalaz.{BloomFilter => _, _}
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._

/**
 * Standard (immutable) bloom filter
 *
 * http://en.wikipedia.org/wiki/Bloom_filter
 */

object StandardBloomFilter{
  def apply[A, B](numItems: Int, fpProb: Double,
                  seed: Long = 0L)(items: A*)
                                  (implicit h: Hashable[A, B],
                                   hconv: HashCodeConverter[B, Int]): StandardBloomFilter[A,B] = {
    val (numHashes, width) = optimalParameters(numItems, fpProb)

    val z: StandardBloomFilter[A,B] = BFZero(numHashes, width, seed)(h, hconv)

    items.foldLeft(z: StandardBloomFilter[A,B])((acc, x) => acc + x)
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

  def monoid[A,B](parameters: (Int, Int), seed: Long = 0L)
                 (implicit h: Hashable[A, B],
                  hconv: HashCodeConverter[B, Int]): Monoid[StandardBloomFilter[A,B]] with Equal[StandardBloomFilter[A,B]] = {
    val (numHashes, width) = parameters
    new Monoid[StandardBloomFilter[A,B]] with Equal[StandardBloomFilter[A,B]]{

        /**
         * Check if parameters are equal - compatibility check
         * Would be nice to make this type safe instead
         **/
      def hasSameParameters(bf1: StandardBloomFilter[A,B],
                            bf2: StandardBloomFilter[A,B]): Boolean = {
        (bf1.numHashes === bf2.numHashes) &&
        (bf1.width === bf2.width) &&
        (bf1.seed === bf2.seed)
      }


      def equal(bf1: StandardBloomFilter[A,B], bf2: StandardBloomFilter[A,B]) = {
        require(hasSameParameters(zero, bf2),
                "bloom filter equality must use Equal with same parameters")
        require(hasSameParameters(bf1, bf2),
                "bloom filters not comparable if parameters are different")

        Tags.Conjunction((bf1, bf2) match {
          case (x1: BFZero[A,B], x2: BFZero[A,B]) => true
          case (x1: BFInstance[A,B], x2: BFInstance[A,B]) => x1.bits == x2.bits
          case (x1: BFZero[A,B], x2: BFInstance[A,B]) => x2.bits.isEmpty
          case (x1: BFInstance[A,B], x2: BFZero[A,B]) => x1.bits.isEmpty
          case _ => false
        })
      }

      override val zero: StandardBloomFilter[A,B] = BFZero[A,B](numHashes, width, seed)(h, hconv)

      def append(bf1: StandardBloomFilter[A,B], bf2: => StandardBloomFilter[A,B]): StandardBloomFilter[A,B] = {
        require(hasSameParameters(bf1, bf2),
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

  def toBitSet(seq: Seq[Int @@ HashCode]) = BitSet(seq : _*)

}

sealed trait StandardBloomFilter[A,B] extends BloomFilter[A,B]{

  override def + (item: A): StandardBloomFilter[A, B]

}
/**
 * Representation of an empty bloom filter
 **/
case class BFZero[A,B](val numHashes: Int,
                       val width: Int,
                       val seed: Long = 0L)
                      (implicit h: Hashable[A, B],
                       hconv: HashCodeConverter[B, Int])
     extends StandardBloomFilter[A, B]{

  def + (item: A): StandardBloomFilter[A, B] =
    BFInstance(numHashes,width,StandardBloomFilter.toBitSet(hashItem(item)),seed)(h,hconv)

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
     extends StandardBloomFilter[A, B]{

  def + (item: A): StandardBloomFilter[A, B] =
    construct(bits ++ StandardBloomFilter.toBitSet(hashItem(item)))

  def contains(item: A): Boolean = {
    val itemBits = StandardBloomFilter.toBitSet(hashItem(item))
                           (bits & itemBits) == itemBits
  }

  private def construct(b: BitSet): StandardBloomFilter[A, B] =
    BFInstance(numHashes, width, b, seed)(h, hconv)
}
