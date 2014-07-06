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

package scalaton.aggregate

import scalaton.util._
import scala.collection.BitSet
import com.googlecode.javaewah.{EWAHCompressedBitmap => CompressedBitSet}

package object hashed {
  implicit class StandardBloomFilterSingletonOps(sbf: StandardBloomFilter.type) extends StandardBloomFilterParameters {
    def empty[A : Hashable32](numHashes: Int, width: Int, seed: Long = 0) = StandardBloomFilter[A](BitSet.empty, numHashes, width, seed)
    def fromData[A : Hashable32](numHashes: Int, width: Int, seed: Long = 0)(xs: Iterable[A]) = xs.foldLeft(empty[A](numHashes, width, seed))(_ + _)
  }

  implicit class SparseBloomFilterSingletonOps(sbf: SparseBloomFilter.type) extends StandardBloomFilterParameters {
    def empty[A : Hashable32](numHashes: Int, width: Int, seed: Long = 0) = SparseBloomFilter(new CompressedBitSet, numHashes, width, seed)
    def fromData[A : Hashable32](numHashes: Int, width: Int, seed: Long = 0)(xs: Iterable[A]) = xs.foldLeft(empty[A](numHashes, width, seed))(_ + _)
  }


  implicit class CountMinSketchSingletonOps(sbf: CountMinSketch.type) extends CountMinSketchParameters {
    def empty[A : Hashable32](numHashes: Int, width: Int, seed: Long = 0) = CountMinSketch(Vector.fill(numHashes, width)(0L), 0, numHashes, width, seed)
    def fromData[A : Hashable32](numHashes: Int, width: Int, seed: Long = 0)(xs: Iterable[(A,Long)]) = xs.foldLeft(empty[A](numHashes, width, seed))((t,x) => t + x)
  }

  implicit class StandardHyperLogLogSingletonOps(sbf: StandardHyperLogLog.type) {
    def empty[A : Hashable32](b: Int, seed: Long = 0) = StandardHyperLogLog(Vector.fill[Byte](1 << (b - 1))(0), b, seed)
    def fromData[A : Hashable32](b: Int, seed: Long = 0)(xs: Iterable[A]) = xs.foldLeft(empty[A](b, seed))(_ + _)
  }
  implicit class SparseHyperLogLogSingletonOps(sbf: SparseHyperLogLog.type) {
    def empty[A : Hashable32](b: Int, seed: Long = 0) = SparseHyperLogLog(Map.empty, b, seed)
    def fromData[A : Hashable32](b: Int, seed: Long = 0)(xs: Iterable[A]) = xs.foldLeft(empty[A](b, seed))(_ + _)
  }
}
