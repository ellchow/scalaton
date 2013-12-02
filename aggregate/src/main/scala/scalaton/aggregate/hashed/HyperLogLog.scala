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
import scalaton.util.hashing._
import scalaton.util.hashing32.Bits32
import scalaton.util.monoids._

// /** Hyper log log implementation using 32 bit hash (http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf).  Good for cardinalities from 0 to 10^9
//   * NOTE: hashing32 does not work well! use hashing64 or hashing128
//   **/

trait HyperLogLogModule extends HashedCollectionModule{
  trait HyperLogLog[D] extends HashedCollection[Bits32,D]{
    def insert[A,H1](d: D, a: A)(implicit h: Hashable[A,H1], hconv: HashCodeConverter[H1,Bits32]): D = {
      val hashedValue = hashItem(d, a).head

      val j = readAddress(d, hashedValue)

      val r = numLeadingZeros(d, hashedValue)

      (registerValue(d, j) lt r) ? updateRegister(d, j, r) | d
    }

    def size(d: D): Long = {
      val rsum = registerSum(d)

      val estimate = alphamm(d) / rsum

      val correctedEstimate = if(estimate lte (5.0 / 2.0 * m(d))){ // small range correction
        val v = numZeroRegisters(d)

        (v =/= 0) ? (m(d) * (math.log(m(d)) - math.log(v))) | estimate
      }else if(estimate gt pow2to32){ // large range correction
        negPow2to32 * math.log(1 - estimate / pow2to32)
       }else{
         estimate
      }

      math.round(correctedEstimate)
    }

    def merge(d1: D, d2: => D): D = {
      require(isCompatible(d1, d2))

      (0 until m(d1)).foldLeft(d1){ (dd, j) =>
        val r = registerValue(d1, j).max((registerValue(d2, j)))

        updateRegister(dd, j, r.toByte)
      }
    }

    def empty(b: Int, s: Long): D

    def fromData[A,H1](b: Int, s: Long)(as: Iterable[A])(implicit ha: Hashable[A,H1], hconv: HashCodeConverter[H1,Bits32]) =
      as.foldLeft(empty(b, s))((dd, a) => insert(dd, a))

    /** b = log2(m) **/
    def b(d: D): Int

    /** number of registers **/
    def m(d: D): Int = 1 << (b(d) - 1)

    def numHashes(d: D): Int = 1

    /** multiplicative constant alpha * m^2 used for computing estimate **/
    protected def alphamm(d: D): Double = b(d) match {
      case 4 => 0.673
      case 5 => 0.697
      case 6 => 0.709
      case _ => (0.7213 / (1 + 1.079 / m(d))) * m(d) * m(d)
    }

    /** number of leading zeros, after skipping the b bits used for addressing **/
    protected def numLeadingZeros(d: D, hashedValue: Bits32): Byte =
    (Integer.numberOfLeadingZeros((hashedValue << b(d)) | (1 << (b(d) - 1)) + 1) + 1).toByte

    /** read the 1st b bits to determing the register address **/
    protected def readAddress(d: D, hashedValue: Bits32): Int =
      hashedValue >> (Integer.SIZE - b(d))

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

  type DenseHyperLogLogData = (Vector[Byte], Int, Long)
  trait DenseHyperLogLog extends HyperLogLog[DenseHyperLogLogData]{
    def seed(d: DenseHyperLogLogData) = d._3

    def b(d: DenseHyperLogLogData) = d._2

    def empty(b: Int, s: Long) = (Vector.fill[Byte](1 << (b - 1))(0), b, s)

    protected def registerSum(d: DenseHyperLogLogData) =
      d._1.map(count => math.pow(2, -count)).sum

    protected def numZeroRegisters(d: DenseHyperLogLogData) =
      d._1.foldLeft(0)((acc, x) => (x === 0) ? (acc + 1) | acc)

    protected def registerValue(d: DenseHyperLogLogData, j: Int) = d._1(j)

    protected def updateRegister(d: DenseHyperLogLogData, j: Int, n: Byte) = d.copy(_1 = d._1.updated(j, n))
  }
  implicit object DenseHyperLogLog extends DenseHyperLogLog

  type SparseHyperLogLogData = (Map[Int, Byte @@ Tags.Max], Int, Long)
  trait SparseHyperLogLog extends HyperLogLog[SparseHyperLogLogData]{
    def seed(d: SparseHyperLogLogData) = d._3

    def b(d: SparseHyperLogLogData) = d._2

    def empty(b: Int, s: Long) = (Map[Int, Byte @@ Tags.Max](), b, s)

    protected def registerSum(d: SparseHyperLogLogData) = {
      val nonZeroCountSum = d._1.map{ case (j, count) => math.pow(2, -count) }.sum

      numZeroRegisters(d) + nonZeroCountSum
    }

    protected def numZeroRegisters(d: SparseHyperLogLogData) =
      m(d) - d._1.size

    protected def registerValue(d: SparseHyperLogLogData, j: Int) = d._1.get(j) | Tags.Max(0)

    protected def updateRegister(d: SparseHyperLogLogData, j: Int, n: Byte) = d.copy(_1 = d._1.updated(j, Tags.Max(n)))
  }
  implicit object SparseHyperLogLog extends SparseHyperLogLog

  object implicits{
    implicit def hyperLogLogSemigroup[D : HyperLogLog] = new Semigroup[D]{
      def append(d1: D, d2: => D) = implicitly[HyperLogLog[D]].merge(d1, d2)
    }

    implicit class HyperLogLogOps[D](val d: D)(implicit hll: HyperLogLog[D]){
      def insert[A,H1](a: A)(implicit h: Hashable[A,H1], hconv: HashCodeConverter[H1,Bits32]): D = hll.insert(d, a)

      def size: Long = hll.size(d)
    }

  }
}

object hyperloglog extends HyperLogLogModule

