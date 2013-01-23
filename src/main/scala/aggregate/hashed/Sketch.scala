package scalaton.aggregate.hashed

import scala.language.higherKinds
import scala.language.postfixOps

import scalaz.{BloomFilter => _, _}
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._


/**
 * Type class for sketches
 * Item A is hashed to B and then to an Int;  T is value container stored
 * in the sketch, and R is the value that can be extracted from T; F is
 * the container of T
 *
 * http://dimacs.rutgers.edu/~graham/pubs/papers/cm-full.pdf
 **/

trait Sketch[A,B,T,R,F]
extends HashedCollection[A,B,Int,F]
with MakesSingleton[A,B,Int,T,F]
with MapLike[A,B,Int,T,R,F]
with Sized[F]


sealed trait CSK

trait CountSketch[A,B,T,R]
extends Sketch[A,B,T,R,(Vector[Vector[T]],Long) @@ CSK]{

  val width: Int

  /** could possibly use double hashing - see BloomFilter.scala **/
  override def hashItem(item: A)(implicit h: Hashable[A, B],
                                 hconv: HashCodeConverter[B, Int]): Iterable[Int @@ HashCode] =
    super.hashItem(item)(h,hconv) map { _ % width |> HashCode}

  /** Given the values R saved in the sketch, produce an estimate**/
  protected def estimate(rs: Iterable[R]): R

  protected def updateValue(t: T, item: A)
                           (implicit mon: Monoid[T]): T

  /** Update the element's value **/
  def insert(data: (Vector[Vector[T]], Long) @@ CSK, item: A)
            (implicit mon: Monoid[T],
             h: Hashable[A, B],
             hconv: HashCodeConverter[B, Int]): (Vector[Vector[T]], Long) @@ CSK = {
    val table = data._1
    val size = data._2

    val idxs = hashItem(item) zipWithIndex

    val newTable = idxs map { case (col, row) =>
                              table(row).updated(col, updateValue(table(row)(col), item)) } toVector

    Tag[(Vector[Vector[T]],Long), CSK]((newTable, size + 1))
  }

  /** Retrieve estimate of the element's value **/
  def get(data: (Vector[Vector[T]], Long) @@ CSK, item: A)
         (implicit v: Value[T,R],
          h: Hashable[A, B],
          hconv: HashCodeConverter[B, Int]): R = {
    val table = data._1
    val size = data._2

    val rs = hashItem(item).zipWithIndex map { case (col, row) =>
                                               v.valueOf(table(row)(col)) }
    estimate(rs)
  }

  /** Number of elements inserted **/
  def cardinality(data: (Vector[Vector[T]], Long) @@ CSK): Long = data._2

}


sealed trait CountMinSketch[A,B]
extends CountSketch[A,B,Long,Long]
with Equal[(Vector[Vector[Long]], Long) @@ CSK]{


  protected def estimate(rs: Iterable[Long]): Long = rs min

  def updateValue(t: Long, item: A)
                 (implicit mon: Monoid[Long]): Long = t |+| 1

}

object sketch
extends HashedCollectionFunctions
with MakesSingletonFunctions
with SetLikeFunctions
with MapLikeFunctions
with SizedFunctions{

  type CMS = (Vector[Vector[Long]], Long) @@ CSK

  object CountMinSketch{

    def apply[A,B](params: (Int, Int), s: Long = 0L) = new CountMinSketch[A,B]{

      val (numHashes, width) = params

      val seed = s

      def equal(cms1: (Vector[Vector[Long]], Long) @@ CSK,
                cms2: (Vector[Vector[Long]], Long) @@ CSK) =
        (cms1._1 == cms2._1) && (cms1._2 == cms2._2)


      val zero: (Vector[Vector[Long]], Long) @@ CSK =
        Tag[(Vector[Vector[Long]], Long), CSK]((Vector.fill[Long](numHashes, width)(0L), 0L))

      def append(cms1: (Vector[Vector[Long]], Long) @@ CSK,
                 cms2: => (Vector[Vector[Long]], Long) @@ CSK): (Vector[Vector[Long]], Long) @@ CSK = {
        val newTable = (0 until numHashes) map { i =>
          (0 until width) map { j => cms1._1(i)(j) + cms2._1(i)(j) } toVector
        } toVector

        Tag[(Vector[Vector[Long]], Long), CSK]((newTable, cms1._2 + cms2._2))
      }
    }

    def optimalParameters(eps: Double, delta: Double) =
      (optimalNumHashes(delta), optimalWidth(eps))

    def optimalNumHashes(delta: Double) =
      math.log(1.0 / delta).toInt

    def optimalWidth(eps: Double) =
      (math.exp(1) / eps).toInt

  }
}
