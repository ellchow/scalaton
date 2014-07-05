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

  def apply[A,B](a: A)(implicit h: Hashable[A,B]) = h.hash(a)

}

object Hash extends HashFunctions with HashableTupleInstances {
  object MurmurHash32Implicits extends Tuple32Instances {
    /*
     val s = """Array[Byte]||Array[Char]||Array[Short]||Array[Int]||Array[Float]||Array[Long]||Array[Double]||Char||Short||Int||Float||Long||Double||CharSequence"""
     List(("32","Int"),("64", "Long"),("128","(Long,Long)")).foreach{ case (yy, t) =>
     s.split("\\|\\|").foreach{ xx =>
     val XX = xx.replaceAll("\\[","").replaceAll("\\]","")
     println(s"""implicit val ${XX}ToMurmurHash${yy}Hashable = new Hashable[${xx},${t}] { def hash(a: ${xx}): ${t} = MurmurHash${yy}Instance.apply(a) }""")
     }}
     */
    type Hashable32[A] = Hashable[A,Int]
    implicit val ArrayByteToMurmurHash32Hashable = new Hashable[Array[Byte],Int] { def hash(a: Array[Byte]): Int = MurmurHash32Instance.apply(a) }
    implicit val ArrayCharToMurmurHash32Hashable = new Hashable[Array[Char],Int] { def hash(a: Array[Char]): Int = MurmurHash32Instance.apply(a) }
    implicit val ArrayShortToMurmurHash32Hashable = new Hashable[Array[Short],Int] { def hash(a: Array[Short]): Int = MurmurHash32Instance.apply(a) }
    implicit val ArrayIntToMurmurHash32Hashable = new Hashable[Array[Int],Int] { def hash(a: Array[Int]): Int = MurmurHash32Instance.apply(a) }
    implicit val ArrayFloatToMurmurHash32Hashable = new Hashable[Array[Float],Int] { def hash(a: Array[Float]): Int = MurmurHash32Instance.apply(a) }
    implicit val ArrayLongToMurmurHash32Hashable = new Hashable[Array[Long],Int] { def hash(a: Array[Long]): Int = MurmurHash32Instance.apply(a) }
    implicit val ArrayDoubleToMurmurHash32Hashable = new Hashable[Array[Double],Int] { def hash(a: Array[Double]): Int = MurmurHash32Instance.apply(a) }
    implicit val CharToMurmurHash32Hashable = new Hashable[Char,Int] { def hash(a: Char): Int = MurmurHash32Instance.apply(a) }
    implicit val ShortToMurmurHash32Hashable = new Hashable[Short,Int] { def hash(a: Short): Int = MurmurHash32Instance.apply(a) }
    implicit val IntToMurmurHash32Hashable = new Hashable[Int,Int] { def hash(a: Int): Int = MurmurHash32Instance.apply(a) }
    implicit val FloatToMurmurHash32Hashable = new Hashable[Float,Int] { def hash(a: Float): Int = MurmurHash32Instance.apply(a) }
    implicit val LongToMurmurHash32Hashable = new Hashable[Long,Int] { def hash(a: Long): Int = MurmurHash32Instance.apply(a) }
    implicit val DoubleToMurmurHash32Hashable = new Hashable[Double,Int] { def hash(a: Double): Int = MurmurHash32Instance.apply(a) }
    implicit val CharSequenceToMurmurHash32Hashable = new Hashable[CharSequence,Int] { def hash(a: CharSequence): Int = MurmurHash32Instance.apply(a) }

    implicit val StringToMurmurHash32Hashable = new Hashable[String,Int] { def hash(a: String): Int = ArrayByteToMurmurHash32Hashable.hash(a.getBytes) }
    implicit def IterableToMurmurHash32Hashable[A : Hashable32, F[A] <: Iterable[A]] = new Hashable[F[A], Int] { def hash(a: F[A]) = combine(a.map(x => Hash.apply(x))) }
  }
  object MurmurHash64Implicits extends Tuple64Instances {
    type Hashable64[A] = Hashable[A,Long]
    implicit val ArrayByteToMurmurHash64Hashable = new Hashable[Array[Byte],Long] { def hash(a: Array[Byte]): Long = MurmurHash64Instance.apply(a) }
    implicit val ArrayCharToMurmurHash64Hashable = new Hashable[Array[Char],Long] { def hash(a: Array[Char]): Long = MurmurHash64Instance.apply(a) }
    implicit val ArrayShortToMurmurHash64Hashable = new Hashable[Array[Short],Long] { def hash(a: Array[Short]): Long = MurmurHash64Instance.apply(a) }
    implicit val ArrayIntToMurmurHash64Hashable = new Hashable[Array[Int],Long] { def hash(a: Array[Int]): Long = MurmurHash64Instance.apply(a) }
    implicit val ArrayFloatToMurmurHash64Hashable = new Hashable[Array[Float],Long] { def hash(a: Array[Float]): Long = MurmurHash64Instance.apply(a) }
    implicit val ArrayLongToMurmurHash64Hashable = new Hashable[Array[Long],Long] { def hash(a: Array[Long]): Long = MurmurHash64Instance.apply(a) }
    implicit val ArrayDoubleToMurmurHash64Hashable = new Hashable[Array[Double],Long] { def hash(a: Array[Double]): Long = MurmurHash64Instance.apply(a) }
    implicit val CharToMurmurHash64Hashable = new Hashable[Char,Long] { def hash(a: Char): Long = MurmurHash64Instance.apply(a) }
    implicit val ShortToMurmurHash64Hashable = new Hashable[Short,Long] { def hash(a: Short): Long = MurmurHash64Instance.apply(a) }
    implicit val IntToMurmurHash64Hashable = new Hashable[Int,Long] { def hash(a: Int): Long = MurmurHash64Instance.apply(a) }
    implicit val FloatToMurmurHash64Hashable = new Hashable[Float,Long] { def hash(a: Float): Long = MurmurHash64Instance.apply(a) }
    implicit val LongToMurmurHash64Hashable = new Hashable[Long,Long] { def hash(a: Long): Long = MurmurHash64Instance.apply(a) }
    implicit val DoubleToMurmurHash64Hashable = new Hashable[Double,Long] { def hash(a: Double): Long = MurmurHash64Instance.apply(a) }
    implicit val CharSequenceToMurmurHash64Hashable = new Hashable[CharSequence,Long] { def hash(a: CharSequence): Long = MurmurHash64Instance.apply(a) }

    implicit val StringToMurmurHash64Hashable = new Hashable[String,Long] { def hash(a: String): Long = ArrayByteToMurmurHash64Hashable.hash(a.getBytes) }
    implicit def IterableToMurmurHash64Hashable[A : Hashable64, F[A] <: Iterable[A]] = new Hashable[F[A], Long] { def hash(a: F[A]) = combine(a.map(x => Hash.apply(x))) }
  }
  object MurmurHash128Instances extends Tuple128Instances {
    type Hashable128[A] = Hashable[A,(Long,Long)]
    implicit val ArrayByteToMurmurHash128Hashable = new Hashable[Array[Byte],(Long,Long)] { def hash(a: Array[Byte]): (Long,Long) = MurmurHash128Instance.apply(a) }
    implicit val ArrayCharToMurmurHash128Hashable = new Hashable[Array[Char],(Long,Long)] { def hash(a: Array[Char]): (Long,Long) = MurmurHash128Instance.apply(a) }
    implicit val ArrayShortToMurmurHash128Hashable = new Hashable[Array[Short],(Long,Long)] { def hash(a: Array[Short]): (Long,Long) = MurmurHash128Instance.apply(a) }
    implicit val ArrayIntToMurmurHash128Hashable = new Hashable[Array[Int],(Long,Long)] { def hash(a: Array[Int]): (Long,Long) = MurmurHash128Instance.apply(a) }
    implicit val ArrayFloatToMurmurHash128Hashable = new Hashable[Array[Float],(Long,Long)] { def hash(a: Array[Float]): (Long,Long) = MurmurHash128Instance.apply(a) }
    implicit val ArrayLongToMurmurHash128Hashable = new Hashable[Array[Long],(Long,Long)] { def hash(a: Array[Long]): (Long,Long) = MurmurHash128Instance.apply(a) }
    implicit val ArrayDoubleToMurmurHash128Hashable = new Hashable[Array[Double],(Long,Long)] { def hash(a: Array[Double]): (Long,Long) = MurmurHash128Instance.apply(a) }
    implicit val CharToMurmurHash128Hashable = new Hashable[Char,(Long,Long)] { def hash(a: Char): (Long,Long) = MurmurHash128Instance.apply(a) }
    implicit val ShortToMurmurHash128Hashable = new Hashable[Short,(Long,Long)] { def hash(a: Short): (Long,Long) = MurmurHash128Instance.apply(a) }
    implicit val IntToMurmurHash128Hashable = new Hashable[Int,(Long,Long)] { def hash(a: Int): (Long,Long) = MurmurHash128Instance.apply(a) }
    implicit val FloatToMurmurHash128Hashable = new Hashable[Float,(Long,Long)] { def hash(a: Float): (Long,Long) = MurmurHash128Instance.apply(a) }
    implicit val LongToMurmurHash128Hashable = new Hashable[Long,(Long,Long)] { def hash(a: Long): (Long,Long) = MurmurHash128Instance.apply(a) }
    implicit val DoubleToMurmurHash128Hashable = new Hashable[Double,(Long,Long)] { def hash(a: Double): (Long,Long) = MurmurHash128Instance.apply(a) }
    implicit val CharSequenceToMurmurHash128Hashable = new Hashable[CharSequence,(Long,Long)] { def hash(a: CharSequence): (Long,Long) = MurmurHash128Instance.apply(a) }

    implicit val StringToMurmurHash128Hashable = new Hashable[String,(Long,Long)] { def hash(a: String): (Long,Long) = ArrayByteToMurmurHash128Hashable.hash(a.getBytes) }
    implicit def IterableToMurmurHash128Hashable[A : Hashable128, F[A] <: Iterable[A]] = new Hashable[F[A], (Long,Long)] { def hash(a: F[A]) = combine(a.map(x => Hash.apply(x))) }
  }

}
