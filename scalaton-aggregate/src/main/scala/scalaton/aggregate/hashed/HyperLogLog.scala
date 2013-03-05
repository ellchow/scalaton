package scalaton.aggregate.hashed

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.hashing._

/** Hyper log log implementation using 32 bit hash (http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf) **/
trait HyperLogLogT[A,H1,T]
extends HashedCollection[A,H1,Bits32]
with InsertsElement[A,H1,Bits32,Vector[Byte] @@ T]
with Sized[A,H1,Bits32,Vector[Byte] @@ T]
with Equal[Vector[Byte] @@ T]
with Monoid[Vector[Byte] @@ T]{

  val numHashes: Int = 1

  val b: Int

  lazy val m = math.pow(2, b) toInt

  private lazy val alphamm = b match {
    case 4 => 0.673
    case 5 => 0.697
    case 6 => 0.709
    case _ => (0.7213 / (1 + 1.079 / m)) * m * m
  }

  protected def tag(d: Vector[Byte]) = Tag[Vector[Byte], T](d)

  private def numLeadingZeros(hashedValue: Bits32) =
    math.min(Byte.MaxValue,
             Integer.numberOfLeadingZeros((hashedValue << b) | (1 << (b - 1)) + 1) + 1) toByte

  private def readAddress(hashedValue: Bits32) =
    hashedValue >> (Integer.SIZE - b)

  def add(d: Vector[Byte] @@ T, a: A)(implicit h: H, hconv: HC): Vector[Byte] @@ T = {
    val hashedValue = hashItem(a).head

    val j = readAddress(hashedValue)

    val r = numLeadingZeros(hashedValue)

    tag((d(j) < r) ? d.updated(j, r) | d)
  }


  lazy val zero = tag(Vector.fill[Byte](m)(0))
}

