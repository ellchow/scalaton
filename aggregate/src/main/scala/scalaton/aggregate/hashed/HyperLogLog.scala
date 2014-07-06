/*
 Copyright 2013 Elliot Chow

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

package scalaton.aggregate.hashed

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.monoids._

case class StandardHyperLogLog[A](data: Vector[Byte], b: Int, seed: Long = 0)(implicit val hashable: Hashable32[A]) extends HyperLogLog[A,Vector[Byte],StandardHyperLogLog[A]] {
  protected def apply(data: Vector[Byte]) = this.copy(data = data)
  protected def registerSum(d: Vector[Byte]) = d.view.map(count => math.pow(2, -count)).sum
  protected def numZeroRegisters(d: Vector[Byte]) = d.count(_ == 0)
  protected def registerValue(d: Vector[Byte], j: Int) = d(j)
  protected def updateRegister(d: Vector[Byte], j: Int, n: Byte) = d.updated(j, n)
}

case class SparseHyperLogLog[A](data: Map[Int, Byte @@ Tags.Max], b: Int, seed: Long = 0)(implicit val hashable: Hashable32[A]) extends HyperLogLog[A,Map[Int, Byte @@ Tags.Max],SparseHyperLogLog[A]] {
  protected def apply(data: Map[Int, Byte @@ Tags.Max]) = this.copy(data = data)
  protected def registerSum(d: Map[Int, Byte @@ Tags.Max]) = {
    val nonZeroCountSum = d.view.map{ case (j, count) => math.pow(2, -count) }.sum
    numZeroRegisters(d) + nonZeroCountSum
  }
  protected def numZeroRegisters(d: Map[Int, Byte @@ Tags.Max]) = m - d.size
  protected def registerValue(d: Map[Int, Byte @@ Tags.Max], j: Int) = d.get(j) | Tags.Max(0)
  protected def updateRegister(d: Map[Int, Byte @@ Tags.Max], j: Int, n: Byte) = d.updated(j, Tags.Max(n))
}
trait HyperLogLog[A,D,T <: HyperLogLog[A,D,T]] extends HashedCollection[A] {
  val data: D
  val b: Int
  /** sum of 2^(-registerValue_j) for all j in 1...m **/
  protected def registerSum(d: D): Double
  /** number of registers with value 0 **/
  protected def numZeroRegisters(d: D): Int
  /** retrieve register value **/
  protected def registerValue(d: D, j: Int): Int
  /** update register value **/
  protected def updateRegister(d: D, j: Int, n: Byte): D
  protected def apply(d: D): T


  /** number of registers **/
  lazy val m: Int = 1 << (b - 1)
  val numHashes: Int = 1
  /** multiplicative constant alpha * m^2 used for computing estimate **/
  protected lazy val alphamm: Double = b match {
    case 4 => 0.673
    case 5 => 0.697
    case 6 => 0.709
    case _ => (0.7213 / (1 + 1.079 / m)) * m * m
  }
  /** number of leading zeros, after skipping the b bits used for addressing **/
  protected def numLeadingZeros(hashedValue: Int): Byte =
    (Integer.numberOfLeadingZeros((hashedValue << b) | (1 << (b - 1)) + 1) + 1).toByte
  /** read the 1st b bits to determing the register address **/
  protected def readAddress(hashedValue: Int): Int =
    hashedValue >>> (Integer.SIZE - b + 1)
  protected val pow2to32 = math.pow(2,32)
  protected val negPow2to32 = -4294967296.0


  def + (a: A): T = {
    val hashedValue = hashItem(a).head

    val j = readAddress(hashedValue)

    val r = numLeadingZeros(hashedValue)

    (registerValue(data, j) lt r) ? apply(updateRegister(data, j, r)) | apply(data)
  }
  def size: Long = {
    val rsum = registerSum(data)

    val estimate = alphamm / rsum

    val correctedEstimate = if (estimate lte (5.0 / 2.0 * m)) { // small range correction
      val v = numZeroRegisters(data)

      (v =/= 0) ? (m * (math.log(m) - math.log(v))) | estimate
    } else if (estimate > pow2to32) { // large range correction
      negPow2to32 * math.log(1 - estimate / pow2to32)
    } else {
      estimate
    }

    math.round(correctedEstimate)
  }

  def merge(x: T): T = {
    require(isCompatible(x))

    val d = (0 until m).foldLeft(data){ (dd, j) =>
      val r = registerValue(data, j).max((registerValue(x.data, j)))

      updateRegister(dd, j, r.toByte)
    }

    apply(d)
  }
}
