package scalaton.aggregate.hashed

import scalaz._
import Scalaz._

import scalaton.util.{Tags => _, _}
import scalaton.util.hashing._
import scalaton.util.monoids._

/** Hyper log log implementation using 32 bit hash (http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf).  Good for cardinalities from 0 to 10^9 **/
trait HyperLogLogT[A,H1,D]
extends HashedCollection[A,H1,Bits32]
with InsertsElement[A,H1,Bits32,D]
with Sized[A,H1,Bits32,D]
with Equal[D]
with Monoid[D]{
  val numHashes: Int = 1

  /** b = log2(m) **/
  val b: Int

  /** number of registers **/
  lazy val m: Int = 1 << (b - 1)

  def insert(d: D, a: A)(implicit h: H, hconv: HC): D = {
    val hashedValue = hashItem(a).head

    val j = readAddress(hashedValue)

    val r = numLeadingZeros(hashedValue)

    (registerValue(d, j) lt r) ? updateRegister(d, j, r) | d
  }

  def cardinality(d: D): Long = {
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

  /** multiplicative constant alpha * m^2 used for computing estimate **/
  protected lazy val alphamm: Double = b match {
    case 4 => 0.673
    case 5 => 0.697
    case 6 => 0.709
    case _ => (0.7213 / (1 + 1.079 / m)) * m * m
  }

  /** number of leading zeros, after skipping the b bits used for addressing **/
  protected def numLeadingZeros(hashedValue: Bits32): Byte =
    (Integer.numberOfLeadingZeros((hashedValue << b) | (1 << (b - 1)) + 1) + 1) toByte

  /** read the 1st b bits to determing the register address **/
  protected def readAddress(hashedValue: Bits32): Int =
    hashedValue >> (Integer.SIZE - b)

  protected val pow2to32 = math.pow(2,32)

  protected val negPow2to32 = -4294967296.0

  /** sum of 2^(-registerValue_j) for all j in 1...m **/
  protected def registerSum(d: D): Double

  /** number of registers with value 0 **/
  protected def numZeroRegisters(d: D): Int

  /** retrieve register value **/
  protected def registerValue(d: D, j: Int): Int

  /** update register value **/
  protected def updateRegister(d: D, j: Int, n: Byte): D

}

trait DenseHyperLogLogT[A,H1,T]
extends HyperLogLogT[A,H1,Vector[Byte] @@ T]{

  def tag(d: Vector[Byte]) = Tag[Vector[Byte], T](d)

  def equal(d1: Vector[Byte] @@ T, d2: Vector[Byte] @@ T) =
    d1 == d2

  lazy val zero = tag(Vector.fill[Byte](m)(0))

  def append (d1: Vector[Byte] @@ T, d2: => Vector[Byte] @@ T): Vector[Byte] @@ T =
    tag(d1.zip(d2) map { case (r1, r2) => r1 max r2 })

  protected def registerSum(d: Vector[Byte] @@ T) =
    d map ( count => math.pow(2, -count)) sum

  protected def numZeroRegisters(d: Vector[Byte] @@ T) =
    d.foldLeft(0)((acc, x) => (x === 0) ? (acc + 1) | acc)

  protected def registerValue(d: Vector[Byte] @@ T, j: Int) = d(j)

  protected def updateRegister(d: Vector[Byte] @@ T, j: Int, n: Byte) = tag(d.updated(j, n))

}

trait SparseHyperLogLogT[A,H1,T]
extends HyperLogLogT[A,H1,Map[Int,Byte @@ Tags.Max] @@ T]{

  def tag(d: Map[Int,Byte @@ Tags.Max]) = Tag[Map[Int,Byte @@ Tags.Max], T](d)

  def equal(d1: Map[Int,Byte @@ Tags.Max] @@ T, d2: Map[Int,Byte @@ Tags.Max] @@ T) =
    d1 == d2

  lazy val zero = tag(Map.empty)

  def append (d1: Map[Int,Byte @@ Tags.Max] @@ T, d2: => Map[Int,Byte @@ Tags.Max] @@ T): Map[Int,Byte @@ Tags.Max] @@ T =
    tag((d1: Map[Int,Byte @@ Tags.Max]) |+| (d2: Map[Int,Byte @@ Tags.Max]))

  protected def registerSum(d: Map[Int,Byte @@ Tags.Max] @@ T) = {
    val nonZeroCountSum = d.map{ case (j, count) => math.pow(2, -count) } sum

    numZeroRegisters(d) + nonZeroCountSum
  }

  protected def numZeroRegisters(d: Map[Int,Byte @@ Tags.Max] @@ T) =
    m - d.size

  protected def registerValue(d: Map[Int,Byte @@ Tags.Max] @@ T, j: Int) = d.get(j) | Tags.Max(0)

  protected def updateRegister(d: Map[Int,Byte @@ Tags.Max] @@ T, j: Int, n: Byte) = tag(d.updated(j, Tags.Max(n)))
}

trait HyperLogLogParameterEstimate{
  def error(m: Int) = 1.04 / math.sqrt(m)
}

object hyperloglog{
  type SparseHLLRegisters[T] = Map[Int,Byte @@ Tags.Max] @@ T

  type DenseHLLRegisters[T] = Vector[Int] @@ T

  object hll
  extends HyperLogLogParameterEstimate{
    def dense[A,H1,T](addressSize: Int, s: Long = 0L) = new DenseHyperLogLogT[A,H1,T]{
      val seed = s

      val b = addressSize
    }

    def sparse[A,H1,T](addressSize: Int, s: Long = 0L) = new SparseHyperLogLogT[A,H1,T]{
      val seed = s

      val b = addressSize
    }
  }
}
