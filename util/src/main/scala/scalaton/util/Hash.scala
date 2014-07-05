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

trait HashFunctions {
  def combine(x: Int, y: Int): Int = 31 * x + y
  def combine(x: Long, y: Long): Long = 31L * x + y
  def combine(x: (Long,Long), y: (Long,Long)): (Long,Long) =
    (combine(x._1, y._1), combine(x._2, y._2))

  def combine(xs: Iterable[Int]): Int = xs.foldLeft(0)(combine _)
  def combine(xs: Iterable[Long]): Long = xs.foldLeft(0L)(combine _)
  def combine(xs: Iterable[(Long,Long)]): (Long, Long) = xs.foldLeft((0L,0L))(combine _)

  def apply[A,B](a: A, seed: Long = 0L)(implicit h: Hashable[A,B]) = h.hash(a,seed)

}

object Hash extends HashFunctions with HashableTupleInstances {
  object MurmurHash32Implicits extends Tuple32Instances {
    /*
     val s = """Array[Byte]||Array[Char]||Array[Short]||Array[Int]||Array[Float]||Array[Long]||Array[Double]||Char||Short||Int||Float||Long||Double||CharSequence"""
     List(("32","Int"),("64", "Long"),("128","(Long,Long)")).foreach{ case (yy, t) =>
     s.split("\\|\\|").foreach{ xx =>
     val XX = xx.replaceAll("\\[","").replaceAll("\\]","")
     println(s"""implicit val ${XX}ToMurmurHash${yy}Hashable = new Hashable[${xx},${t}] { def hash(a: ${xx}, seed: Long): ${t} = MurmurHash${yy}.apply(a, seed) }""")
     }}
     */
    implicit val ArrayByteToMurmurHash32Hashable = new Hashable[Array[Byte],Int] { def hash(a: Array[Byte], seed: Long): Int = MurmurHash32.apply(a, seed) }
    implicit val ArrayCharToMurmurHash32Hashable = new Hashable[Array[Char],Int] { def hash(a: Array[Char], seed: Long): Int = MurmurHash32.apply(a, seed) }
    implicit val ArrayShortToMurmurHash32Hashable = new Hashable[Array[Short],Int] { def hash(a: Array[Short], seed: Long): Int = MurmurHash32.apply(a, seed) }
    implicit val ArrayIntToMurmurHash32Hashable = new Hashable[Array[Int],Int] { def hash(a: Array[Int], seed: Long): Int = MurmurHash32.apply(a, seed) }
    implicit val ArrayFloatToMurmurHash32Hashable = new Hashable[Array[Float],Int] { def hash(a: Array[Float], seed: Long): Int = MurmurHash32.apply(a, seed) }
    implicit val ArrayLongToMurmurHash32Hashable = new Hashable[Array[Long],Int] { def hash(a: Array[Long], seed: Long): Int = MurmurHash32.apply(a, seed) }
    implicit val ArrayDoubleToMurmurHash32Hashable = new Hashable[Array[Double],Int] { def hash(a: Array[Double], seed: Long): Int = MurmurHash32.apply(a, seed) }
    implicit val CharToMurmurHash32Hashable = new Hashable[Char,Int] { def hash(a: Char, seed: Long): Int = MurmurHash32.apply(a, seed) }
    implicit val ShortToMurmurHash32Hashable = new Hashable[Short,Int] { def hash(a: Short, seed: Long): Int = MurmurHash32.apply(a, seed) }
    implicit val IntToMurmurHash32Hashable = new Hashable[Int,Int] { def hash(a: Int, seed: Long): Int = MurmurHash32.apply(a, seed) }
    implicit val FloatToMurmurHash32Hashable = new Hashable[Float,Int] { def hash(a: Float, seed: Long): Int = MurmurHash32.apply(a, seed) }
    implicit val LongToMurmurHash32Hashable = new Hashable[Long,Int] { def hash(a: Long, seed: Long): Int = MurmurHash32.apply(a, seed) }
    implicit val DoubleToMurmurHash32Hashable = new Hashable[Double,Int] { def hash(a: Double, seed: Long): Int = MurmurHash32.apply(a, seed) }
    implicit val CharSequenceToMurmurHash32Hashable = new Hashable[CharSequence,Int] { def hash(a: CharSequence, seed: Long): Int = MurmurHash32.apply(a, seed) }

    implicit val StringToMurmurHash32Hashable = new Hashable[String,Int] { def hash(a: String, seed: Long): Int = ArrayByteToMurmurHash32Hashable.hash(a.getBytes, seed) }
    implicit def IterableToMurmurHash32Hashable[A : Hashable32, F[A] <: Iterable[A]] = new Hashable[F[A], Int] { def hash(a: F[A], seed: Long) = combine(a.map(x => Hash.apply(x, seed))) }
  }
  object MurmurHash64Implicits extends Tuple64Instances {
    implicit val ArrayByteToMurmurHash64Hashable = new Hashable[Array[Byte],Long] { def hash(a: Array[Byte], seed: Long): Long = MurmurHash64.apply(a, seed) }
    implicit val ArrayCharToMurmurHash64Hashable = new Hashable[Array[Char],Long] { def hash(a: Array[Char], seed: Long): Long = MurmurHash64.apply(a, seed) }
    implicit val ArrayShortToMurmurHash64Hashable = new Hashable[Array[Short],Long] { def hash(a: Array[Short], seed: Long): Long = MurmurHash64.apply(a, seed) }
    implicit val ArrayIntToMurmurHash64Hashable = new Hashable[Array[Int],Long] { def hash(a: Array[Int], seed: Long): Long = MurmurHash64.apply(a, seed) }
    implicit val ArrayFloatToMurmurHash64Hashable = new Hashable[Array[Float],Long] { def hash(a: Array[Float], seed: Long): Long = MurmurHash64.apply(a, seed) }
    implicit val ArrayLongToMurmurHash64Hashable = new Hashable[Array[Long],Long] { def hash(a: Array[Long], seed: Long): Long = MurmurHash64.apply(a, seed) }
    implicit val ArrayDoubleToMurmurHash64Hashable = new Hashable[Array[Double],Long] { def hash(a: Array[Double], seed: Long): Long = MurmurHash64.apply(a, seed) }
    implicit val CharToMurmurHash64Hashable = new Hashable[Char,Long] { def hash(a: Char, seed: Long): Long = MurmurHash64.apply(a, seed) }
    implicit val ShortToMurmurHash64Hashable = new Hashable[Short,Long] { def hash(a: Short, seed: Long): Long = MurmurHash64.apply(a, seed) }
    implicit val IntToMurmurHash64Hashable = new Hashable[Int,Long] { def hash(a: Int, seed: Long): Long = MurmurHash64.apply(a, seed) }
    implicit val FloatToMurmurHash64Hashable = new Hashable[Float,Long] { def hash(a: Float, seed: Long): Long = MurmurHash64.apply(a, seed) }
    implicit val LongToMurmurHash64Hashable = new Hashable[Long,Long] { def hash(a: Long, seed: Long): Long = MurmurHash64.apply(a, seed) }
    implicit val DoubleToMurmurHash64Hashable = new Hashable[Double,Long] { def hash(a: Double, seed: Long): Long = MurmurHash64.apply(a, seed) }
    implicit val CharSequenceToMurmurHash64Hashable = new Hashable[CharSequence,Long] { def hash(a: CharSequence, seed: Long): Long = MurmurHash64.apply(a, seed) }

    implicit val StringToMurmurHash64Hashable = new Hashable[String,Long] { def hash(a: String, seed: Long): Long = ArrayByteToMurmurHash64Hashable.hash(a.getBytes, seed) }
    implicit def IterableToMurmurHash64Hashable[A : Hashable64, F[A] <: Iterable[A]] = new Hashable[F[A], Long] { def hash(a: F[A], seed: Long) = combine(a.map(x => Hash.apply(x, seed))) }
  }
  object MurmurHash128s extends Tuple128Instances {
    implicit val ArrayByteToMurmurHash128Hashable = new Hashable[Array[Byte],(Long,Long)] { def hash(a: Array[Byte], seed: Long): (Long,Long) = MurmurHash128.apply(a, seed) }
    implicit val ArrayCharToMurmurHash128Hashable = new Hashable[Array[Char],(Long,Long)] { def hash(a: Array[Char], seed: Long): (Long,Long) = MurmurHash128.apply(a, seed) }
    implicit val ArrayShortToMurmurHash128Hashable = new Hashable[Array[Short],(Long,Long)] { def hash(a: Array[Short], seed: Long): (Long,Long) = MurmurHash128.apply(a, seed) }
    implicit val ArrayIntToMurmurHash128Hashable = new Hashable[Array[Int],(Long,Long)] { def hash(a: Array[Int], seed: Long): (Long,Long) = MurmurHash128.apply(a, seed) }
    implicit val ArrayFloatToMurmurHash128Hashable = new Hashable[Array[Float],(Long,Long)] { def hash(a: Array[Float], seed: Long): (Long,Long) = MurmurHash128.apply(a, seed) }
    implicit val ArrayLongToMurmurHash128Hashable = new Hashable[Array[Long],(Long,Long)] { def hash(a: Array[Long], seed: Long): (Long,Long) = MurmurHash128.apply(a, seed) }
    implicit val ArrayDoubleToMurmurHash128Hashable = new Hashable[Array[Double],(Long,Long)] { def hash(a: Array[Double], seed: Long): (Long,Long) = MurmurHash128.apply(a, seed) }
    implicit val CharToMurmurHash128Hashable = new Hashable[Char,(Long,Long)] { def hash(a: Char, seed: Long): (Long,Long) = MurmurHash128.apply(a, seed) }
    implicit val ShortToMurmurHash128Hashable = new Hashable[Short,(Long,Long)] { def hash(a: Short, seed: Long): (Long,Long) = MurmurHash128.apply(a, seed) }
    implicit val IntToMurmurHash128Hashable = new Hashable[Int,(Long,Long)] { def hash(a: Int, seed: Long): (Long,Long) = MurmurHash128.apply(a, seed) }
    implicit val FloatToMurmurHash128Hashable = new Hashable[Float,(Long,Long)] { def hash(a: Float, seed: Long): (Long,Long) = MurmurHash128.apply(a, seed) }
    implicit val LongToMurmurHash128Hashable = new Hashable[Long,(Long,Long)] { def hash(a: Long, seed: Long): (Long,Long) = MurmurHash128.apply(a, seed) }
    implicit val DoubleToMurmurHash128Hashable = new Hashable[Double,(Long,Long)] { def hash(a: Double, seed: Long): (Long,Long) = MurmurHash128.apply(a, seed) }
    implicit val CharSequenceToMurmurHash128Hashable = new Hashable[CharSequence,(Long,Long)] { def hash(a: CharSequence, seed: Long): (Long,Long) = MurmurHash128.apply(a, seed) }

    implicit val StringToMurmurHash128Hashable = new Hashable[String,(Long,Long)] { def hash(a: String, seed: Long): (Long,Long) = ArrayByteToMurmurHash128Hashable.hash(a.getBytes, seed) }
    implicit def IterableToMurmurHash128Hashable[A : Hashable128, F[A] <: Iterable[A]] = new Hashable[F[A], (Long,Long)] { def hash(a: F[A], seed: Long) = combine(a.map(x => Hash.apply(x, seed))) }
  }

}
