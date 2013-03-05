package scalaton.aggregate.hashed

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.hashing._

/** Hyper log log implementation using 32 bit hash (http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf).  Good for cardinalities from 0 to 10^9 **/
trait HyperLogLogT[A,H1,D,T]
extends HashedCollection[A,H1,Bits32]
with InsertsElement[A,H1,Bits32,D @@ T]
with Sized[A,H1,Bits32,D @@ T]
with Equal[D @@ T]
with Monoid[D @@ T]{
  val numHashes: Int = 1

  val b: Int

  lazy val m: Int = 1 << (b - 1)

  protected lazy val alphamm: Double = b match {
    case 4 => 0.673
    case 5 => 0.697
    case 6 => 0.709
    case _ => (0.7213 / (1 + 1.079 / m)) * m * m
  }

  protected def tag(d: D) = Tag[D, T](d)

  protected def numLeadingZeros(hashedValue: Bits32): Int =
    Integer.numberOfLeadingZeros((hashedValue << b) | (1 << (b - 1)) + 1) + 1

  protected def readAddress(hashedValue: Bits32): Int =
    hashedValue >> (Integer.SIZE - b)

  protected val pow2to32 = math.pow(2,32)

  protected val negPow2to32 = -4294967296.0

  protected def registerSum(d: D @@ T): Double

  protected def numZeroRegisters(d: D @@ T): Int

  def cardinality(d: D @@ T): Long = {
    val rsum = registerSum(d)

    val estimate = alphamm / rsum

    val correctedEstimate = if(estimate lte (5.0 / 2.0 * m)){ // small range correction
      val v = numZeroRegisters(d)

      (v =/= 0) ? (m * (math.log(m) - math.log(v))) | estimate
    }else if(estimate gt pow2to32) // large range correction
      negPow2to32 * math.log(1 - estimate / pow2to32)
    else
      estimate

    math.round(correctedEstimate)
  }



}



trait DenseHyperLogLogT[A,H1,T]
extends HyperLogLogT[A,H1,Vector[Int],T]{

  def add(d: Vector[Int] @@ T, a: A)(implicit h: H, hconv: HC): Vector[Int] @@ T = {
    val hashedValue = hashItem(a).head

    val j = readAddress(hashedValue)

    val r = numLeadingZeros(hashedValue)

    tag((d(j) lt r) ? d.updated(j, r) | d)
  }

  def equal(d1: Vector[Int] @@ T, d2: Vector[Int] @@ T) =
    d1 == d2

  lazy val zero = tag(Vector.fill[Int](m)(0))

  def append (d1: Vector[Int] @@ T, d2: => Vector[Int] @@ T): Vector[Int] @@ T =
    tag(d1.zip(d2) map { case (r1, r2) => r1 max r2 })

  protected def registerSum(d: Vector[Int] @@ T) =
    d map ( count => math.pow(2, (-1 * count))) sum

  protected def numZeroRegisters(d: Vector[Int] @@ T) =
    d.foldLeft(0)((acc, x) => (x === 0) ? (acc + 1) | acc)

}

object hyperloglog{
  type HLLRegisters[T] = Vector[Int] @@ T

  object hll{
    def dense[A,H1,T](addressSize: Int, s: Long = 0L) = new DenseHyperLogLogT[A,H1,T]{
      val seed = s

      val b = addressSize
    }
  }
}
