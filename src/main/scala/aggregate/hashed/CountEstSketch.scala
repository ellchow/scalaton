/*

package scalaton.aggregate.hashed

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._

trait CountEstSketch[A,B,T,R,F]
extends Sketch[A,B,T,R,F]
{

  val width: Int

  /** could possibly use double hashing - see BloomFilter.scala **/
  override def hashItem(item: A)(implicit h: Hashable[A, B],
                                 hconv: HashCodeConverter[B, Int]): Iterable[Int @@ HashCode] =
    super.hashItem(item) map { _ % width |> HashCode}

}


trait CountMinSketchInstances{

  object CountMinSketch{

    sealed trait CMSData

    type CMS = (Vector[Vector[Long]], Long) @@ CMSData
    def CMS(x: (Vector[Vector[Long]], Long) ) = Tag[(Vector[Vector[Long]], Long), CMSData](x)

    def apply[A,B](params: (Int,Int), s: Long = 0L) =
      new CountEstSketch[A,B,Long,Long,CMS]{
        val (numHashes, width) = params

        val seed: Long = s

        def equal(cms1: CMS, cms2: CMS) = (cms1._1 == cms2._1) && (cms1._2 == cms2._2)

        val zero: CMS = CMS((Vector.fill[Long](numHashes, width)(0L), 0L))

        def append(cms1: CMS, cms2: => CMS): CMS = {
          val table = Vector.tabulate(numHashes, width)((i,j) =>
            cms1._1(i)(j) + cms2._1(i)(j) )

          CMS((table, cms1._2 + cms2._2))
        }

        def get(f: CMS, item: A)(implicit v: Value[Long,Long],
                                 h: Hashable[A, B],
                                 hconv: HashCodeConverter[B, Int]): Long =
        (cellsOf(item) map { case (i,j) => valueAt(f, i, j) }) min

        def update(f: CMS , item: A, u: Long)
                  (implicit mon: Monoid[Long],
                   h: Hashable[A, B],
                   hconv: HashCodeConverter[B, Int]): CMS = {
          val cells = cellsOf(item) toSet

          val table = Vector.tabulate(numHashes, width)((i,j) =>
            valueAt(f,i,j) + (cells.contains((i,j)) ? u | 0)
          )

          CMS((table, f._2))
        }

        def cardinality(cms: CMS): Long = cms._2

        protected def cellsOf(item: A)(implicit h: Hashable[A, B],
                                       hconv: HashCodeConverter[B, Int]): Iterable[(Int,Int)] =
        (0 to numHashes).view zip (hashItem(item))

        protected def valueAt(f: CMS, i: Int, j: Int): Long =
          f._1(i)(j)

      }

    def optimalParameters(eps: Double, delta: Double) = (optimalNumHashes(delta), optimalWidth(eps))

    /** delta is certainty having less than eps **/
    def optimalNumHashes(delta: Double) = {
      require((delta gte 0.0) && (delta lte 1.0), "delta must be between 0 and 1")
      math.ceil(math.log(1 - delta) / math.log(0.5)) toInt
    }
    /** eps is max tolerable error **/
    def optimalWidth(eps: Double) = {
      require((eps gte 0.0) && (eps lte 1.0), "eps must be between 0 and 1")
      math.ceil(2 / eps) toInt
    }

  }
}
*/
