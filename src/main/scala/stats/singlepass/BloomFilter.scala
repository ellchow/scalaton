package scalaton.stats.singlepass

import scala.language.postfixOps

import scala.collection.BitSet

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._

// case class BloomFilterStats[A, B]
object bloomfilter{

  val emptyBitSet = BitSet.empty

  def bfmInstance[A,B](numItems: Int, fpProb: Double)
                      (implicit h: Hashable[A, B],
                       hconv: HashCodeConverter[B, Int]): Monoid[BloomFilter[A,B]] with Equal[BloomFilter[A,B]] = {
    val (numHashes, width) = BloomFilter.optimalParameters(numItems, fpProb)
    bfmInstance(numHashes, width)(h, hconv)
  }

  def bfmInstance[A,B](numHashes: Int, width: Int)
                      (implicit h: Hashable[A, B],
                       hconv: HashCodeConverter[B, Int]): Monoid[BloomFilter[A,B]] with Equal[BloomFilter[A,B]] =
    new Monoid[BloomFilter[A,B]] with Equal[BloomFilter[A,B]]{
      def equal(bf1: BloomFilter[A,B], bf2: BloomFilter[A,B]) =
        bf1.hasSameParameters(bf2) && (bf1.bits == bf2.bits)

      def zero = BFZero[A,B](numHashes, width)(h, hconv)

      def append(bf1: BloomFilter[A,B], bf2: => BloomFilter[A,B]): BloomFilter[A,B] =
        bf1 ++ bf2
    }

  sealed trait BloomFilter[A,B]{
    val numHashes: Int
    val width: Int
    val s: Long
    val bits: BitSet

    def hasSameParameters(other: BloomFilter[A,B]) =
      (numHashes == other.numHashes) && (width == other.width) && (s == other.s)

    def + (item: A): BloomFilter[A, B]

    def ++ (other: BloomFilter[A, B]): BloomFilter[A,B]

    def contains(item: A): Boolean
  }

  case class BFZero[A,B](val numHashes: Int,
                         val width: Int,
                         val s: Long = 0L) // cannot put Long @@ HashSeed for some reason...)
                        (implicit h: Hashable[A, B],
                         hconv: HashCodeConverter[B, Int])

  extends BloomFilter[A, B]{
    val bits = emptyBitSet

    def + (item: A): BloomFilter[A, B] =
      BFInstance(numHashes,width,s=s)(h,hconv) + item


    def ++ (other: BloomFilter[A, B]) = {
      require(hasSameParameters(other))
      other
    }

    def contains(item: A): Boolean = false
  }

  case class BFInstance[A, B](val numHashes: Int,
                              val width: Int,
                              val bits: BitSet = emptyBitSet,
                              val s: Long = 0L // cannot put Long @@ HashSeed for some reason...
                            )
                             (implicit h: Hashable[A, B],
                              hconv: HashCodeConverter[B, Int])
       extends BloomFilter[A, B]
  {

    def seed: Long @@ HashSeed = HashSeed(s)

    def hashItem(item: A): Seq[Int @@ HashCode] = {
      val hcs = (multiHash(item, seed)(h) |> hconv.convertSeq) take numHashes
      hcs.toSeq map { _ % width |> HashCode }
    }

    def + (item: A): BloomFilter[A, B] = {
      val hcs = hashItem(item)
      val itemBits: BitSet = BitSet(hcs : _*)
      construct(bits ++ itemBits)
    }

    def ++ (other: BloomFilter[A, B]) = other match {
      case z : BFZero[A,B] => this
      case bf: BFInstance[A,B] => {
        require(hasSameParameters(other))
        construct(bits ++ bf.bits)
      }
    }

    def contains(item: A): Boolean = {
      val hcs = hashItem(item)
      val itemBits: BitSet = BitSet(hcs : _*)

      (bits & itemBits) == itemBits
    }

    private def construct(b: BitSet): BloomFilter[A, B] =
      BFInstance(numHashes, width, b, s)(h, hconv)
  }

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


    def optimalNumHashes(numItems: Int, width: Int): Int =
      math.ceil(width / numItems * math.log(2)).toInt + 1

    def optimalWidth(numItems: Int, fpProb: Double): Int =
      math.ceil(-1 * numItems * math.log(fpProb) / math.log(2) / math.log(2)).toInt + 1

  }

}

