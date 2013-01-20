package scalaton.stats.singlepass

import scala.language.postfixOps

import scala.collection.BitSet

import scalaz.{BloomFilter => _, _}
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._

object bloomfilter{

  def BloomFilter[A, B](numItems: Int, fpProb: Double,
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

  def optimalNumHashes(numItems: Int, width: Int): Int =
    math.ceil(width / numItems * math.log(2)).toInt + 1

  def optimalWidth(numItems: Int, fpProb: Double): Int =
    math.ceil(-1 * numItems * math.log(fpProb) / math.log(2) / math.log(2)).toInt + 1


  def BloomFilterMonoid[A,B](numHashes: Int, width: Int, seed: Long = 0L)
                            (implicit h: Hashable[A, B],
                             hconv: HashCodeConverter[B, Int]): Monoid[BloomFilter[A,B]] with Equal[BloomFilter[A,B]] =
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
      def zero: BloomFilter[A,B] = BFZero[A,B](numHashes, width, seed)(h, hconv)

      def append(bf1: BloomFilter[A,B], bf2: => BloomFilter[A,B]): BloomFilter[A,B] =
        bf1 ++ bf2
    }

}

sealed trait BloomFilter[A,B]{
  val numHashes: Int

  val width: Int

  val seed: Long

  def hasSameParameters(other: BloomFilter[A,B]) =
  (numHashes === other.numHashes) && (width === other.width) && (seed === other.seed)

  def hashItem(item: A)(implicit h: Hashable[A, B],
                        hconv: HashCodeConverter[B, Int]): BitSet = {
    val hcs = (multiHash(item, seed)(h) |> hconv.convertSeq) take numHashes
    BitSet(hcs.toSeq map { _ % width |> HashCode } : _*)
  }

  def + (item: A): BloomFilter[A, B]

  def ++ (other: BloomFilter[A, B]): BloomFilter[A,B]

  def contains(item: A): Boolean
}

case class BFZero[A,B](val numHashes: Int,
                       val width: Int,
                       val seed: Long = 0L)
                      (implicit h: Hashable[A, B],
                       hconv: HashCodeConverter[B, Int])
     extends BloomFilter[A, B]{

  def + (item: A): BloomFilter[A, B] =
    BFInstance(numHashes,width,hashItem(item),seed)(h,hconv)


  def ++ (other: BloomFilter[A, B]) = {
    require(hasSameParameters(other),
            "cannot combine bloom filters with different parameters")
    other
  }

  def contains(item: A): Boolean = false
}

case class BFInstance[A, B](val numHashes: Int,
                            val width: Int,
                            val bits: BitSet,
                            val seed: Long = 0L
                          )
                           (implicit h: Hashable[A, B],
                            hconv: HashCodeConverter[B, Int])
     extends BloomFilter[A, B]
{

  def + (item: A): BloomFilter[A, B] =
    construct(bits ++ hashItem(item))


  def ++ (other: BloomFilter[A, B]) = other match {
    case z : BFZero[A,B] => this
    case bf: BFInstance[A,B] => {
      require(hasSameParameters(other))
      construct(bits ++ bf.bits)
    }
  }

  def contains(item: A): Boolean = {
    val itemBits = hashItem(item)
                           (bits & itemBits) == itemBits
  }

  private def construct(b: BitSet): BloomFilter[A, B] =
    BFInstance(numHashes, width, b, seed)(h, hconv)
}


// trait BloomFilterMonoidInstances extends BloomFilterImpl{
//   /*
//    def bfmInstance[A,B](numItems: Int, fpProb: Double, s: Long = 0L)
//    (implicit h: Hashable[A, B],
//    hconv: HashCodeConverter[B, Int]): Monoid[BloomFilter[A,B]] with Equal[BloomFilter[A,B]] = {
//    val (numHashes, width) = BloomFilter.optimalParameters(numItems, fpProb)
//    bfmInstance(numHashes, width, s)(h, hconv)
//    }
//    */


// }



