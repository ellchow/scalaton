/*
 Copyright 2014 Elliot Chow

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

package scalaton.util

import scalaz._, Scalaz._
import java.nio.ByteBuffer

trait MurmurHash[A]{
  def apply(buffer: ByteBuffer, offset: Int, length: Int, seed: Long): A

  def apply(bytes: Array[Byte], seed: Long): A = apply(ByteBuffer.wrap(bytes), 0, bytes.length, seed)
  def apply(maxBytes: Int, fn: ByteBuffer => Unit, seed: Long): A = {
    val buffer = ByteBuffer.allocate(maxBytes)
    fn(buffer)
    apply(buffer, 0, maxBytes, seed)
  }
  def apply(array: Array[Char], seed: Long): A = apply(array.size * 2, {_.asCharBuffer.put(array)}, seed)
  def apply(array: Array[Short], seed: Long): A = apply(array.size * 2, {_.asShortBuffer.put(array)}, seed)
  def apply(array: Array[Int], seed: Long): A = apply(array.size * 4, {_.asIntBuffer.put(array)}, seed)
  def apply(array: Array[Float], seed: Long): A = apply(array.size * 4, {_.asFloatBuffer.put(array)}, seed)
  def apply(array: Array[Long], seed: Long): A = apply(array.size * 8, {_.asLongBuffer.put(array)}, seed)
  def apply(array: Array[Double], seed: Long): A = apply(array.size * 8, {_.asDoubleBuffer.put(array)}, seed)

  def apply(value: Char, seed: Long): A = apply(2, {_.asCharBuffer.put(value)}, seed)
  def apply(value: Short, seed: Long): A = apply(2, {_.asShortBuffer.put(value)}, seed)
  def apply(value: Int, seed: Long): A = apply(4, {_.asIntBuffer.put(value)}, seed)
  def apply(value: Float, seed: Long): A = apply(4, {_.asFloatBuffer.put(value)}, seed)
  def apply(value: Long, seed: Long): A = apply(8, {_.asLongBuffer.put(value)}, seed)
  def apply(value: Double, seed: Long): A = apply(8, {_.asDoubleBuffer.put(value)}, seed)

  def apply(string : CharSequence, seed: Long): A = apply(string.length * 2, {buffer =>
    val charBuffer = buffer.asCharBuffer
    0.to(string.length - 1).foreach{i => charBuffer.put(string.charAt(i))}
  }, seed)
}
object MurmurHash32 extends MurmurHash[Int]{
  def apply(buffer : ByteBuffer, offset : Int, length : Int, seed: Long): Int =
    CassandraMurmurHash.hash32(buffer, offset, length, seed.toInt)
}

object MurmurHash64 extends MurmurHash[Long]{
  def apply(buffer : ByteBuffer, offset : Int, length : Int, seed: Long): Long =
    CassandraMurmurHash.hash2_64(buffer, offset, length, seed)
}

object MurmurHash128 extends MurmurHash[(Long, Long)]{
  def apply(buffer : ByteBuffer, offset : Int, length : Int, seed: Long) : (Long, Long) = {
    val longs = CassandraMurmurHash.hash3_x64_128(buffer, offset, length, seed)

    (longs(0), longs(1))
  }
}


// class MurmurHash32(seed: Int) extends MurmurHash[Int]{
//   def apply(buffer : ByteBuffer, offset : Int, length : Int): Int =
//     CassandraMurmurHash.hash32(buffer, offset, length, seed)
// }

// class MurmurHash64(seed: Int) extends MurmurHash[Long]{
//   def apply(buffer : ByteBuffer, offset : Int, length : Int): Long =
//     CassandraMurmurHash.hash2_64(buffer, offset, length, seed)
// }

// class MurmurHash128(seed : Long) extends MurmurHash[(Long, Long)]{
//   def apply(buffer : ByteBuffer, offset : Int, length : Int) : (Long, Long) = {
//     val longs = CassandraMurmurHash.hash3_x64_128(buffer, offset, length, seed)

//     (longs(0), longs(1))
//   }
// }
// object MurmurHash32Instance extends MurmurHash32(0)
// object MurmurHash128Instance extends MurmurHash128(0L)
// object MurmurHash64Instance extends MurmurHash64(0)
