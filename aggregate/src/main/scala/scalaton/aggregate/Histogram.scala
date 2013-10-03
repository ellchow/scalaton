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

import collection.immutable.TreeMap
import collection.mutable

import scalaz._
import Scalaz._

import spire.math.Numeric
import moments._

/** implementation of online histogram building algorithm as described by http://jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf **/

/** value associated with a bucket in the histogram - must be able to retrieve the number of items **/
abstract class HistogramValue[A : Monoid]{
  // extract number of items
  def count(a: A): Long
}

/** averaging a target value at a particular point **/
abstract class TargetAverage[Y : Monoid]{
  def weighted(y: Y, w: Double): Y

  def average(y1: Y, y2: Y, w: Double): Y = {
    require((w gte 0.0) && (w lte 1.0))

    weighted(y1, w) |+| weighted(y2, (1 - w))
  }
}

/** object to be inserted into the histogram - must be able to retrieve the representation of point
 *  as well as the value associated with the point
 **/
abstract class HistogramPoint[A, B : HistogramValue : Monoid]{
  def point(a: A): Double

  def value(a: A): B
}

/** wrapper class holding actual histogram buckets and values **/
sealed abstract class HistogramData[A, B : HistogramValue : Monoid]{
  val buckets: TreeMap[Double, B]

  val min: Double

  val max: Double

  lazy val size = buckets.values.map(v => implicitly[HistogramValue[B]].count(v)).sum
}

case class HistogramDataLTE1[A, B : HistogramValue : Monoid](val buckets: TreeMap[Double, B], val min: Double, val max: Double) extends HistogramData[A,B]
case class HistogramDataN[A, B : HistogramValue : Monoid](val buckets: TreeMap[Double, B], val min: Double, val max: Double) extends HistogramData[A,B]


abstract class Histogram[A, B, T](val maxBuckets: Int)(implicit mon: Monoid[B], hv: HistogramValue[B], hp: HistogramPoint[A,B]){
  require(maxBuckets gt 0)

  /** empty histogram **/
  val empty: HistogramData[A,B] @@ T = Tag(HistogramDataLTE1[A,B](TreeMap[Double,B](), Double.PositiveInfinity, Double.NegativeInfinity))

  def apply(as: A*) = insert(empty, as)

  /** gap size for deciding which buckets are closest (to be merged) **/
  def gapSize(x: (Double, Long), y: (Double, Long)): Double //= y._1 - x._1

  /** insert a point into the histogram **/
  def insert(h: HistogramData[A,B] @@ T, a: A): HistogramData[A,B] @@ T = {
    val pDouble = hp.point(a)
    merge(h, Tag(HistogramDataLTE1[A,B](TreeMap(pDouble -> hp.value(a)), pDouble, pDouble)) )
  }

  /** insert a point into the histogram n times **/
  def insertN(h: HistogramData[A,B] @@ T, a: A, n: Int): HistogramData[A,B] @@ T = {
    require(n gt 0)

    (1 to n).foldLeft(h){ case (hh, _) => insert(hh, a) }
  }

  def insert(h: HistogramData[A,B] @@ T, as: Iterable[A]): HistogramData[A,B] @@ T =
    as.foldLeft(h){ case (hh, a) => insert(hh, a) }

  def insert(h: HistogramData[A,B] @@ T, as: A*): HistogramData[A,B] @@ T =
    insert(h, as)

  /** merge 2 histograms together **/
  def merge(h1: HistogramData[A,B] @@ T, h2: HistogramData[A,B] @@ T): HistogramData[A,B] @@ T = {
    val unmergedBuckets = implicitly[Monoid[Map[Double, B]]].append((h1.buckets : Map[Double, B]), (h2.buckets : Map[Double, B])).asInstanceOf[TreeMap[Double, B]]

    @annotation.tailrec
    def loop(bs: TreeMap[Double, B]): TreeMap[Double, B] = {
      if(bs.size lte maxBuckets)
        bs
      else{
        val (_, (k1, k2)) = ((Double.PositiveInfinity, (0.0, 0.0)) /: bs.view.zip(bs.view.drop(1))) {
          case (minSoFar, ((k1,v1), (k2,v2))) =>
            val w = gapSize((k1, hv.count(v1)),
                            (k2, hv.count(v2)))

            (w lt minSoFar._1) ? (w, (k1, k2)) | minSoFar
        }

        val v1 = bs(k1)
        val v2 = bs(k2)

        val k = (k1 * hv.count(v1) + k2 * hv.count(v2)) / (hv.count(v1) + hv.count(v2))
        val v = mon.append(v1,v2)

        loop(bs - k1 - k2 + (k -> v))
      }
    }

    val mergedBuckets = loop(unmergedBuckets)
    val mergedMin = math.min(h1.min, h2.min)
    val mergedMax = math.max(h1.max, h2.max)

    Tag(if(mergedBuckets.size gte 2) HistogramDataN(mergedBuckets, mergedMin, mergedMax) else HistogramDataLTE1(mergedBuckets, mergedMin, mergedMax))
  }

  /** get buckets (in ascending order) from histogram up to buckets that contain counts for the point **/
  def upTo(h: HistogramData[A,B] @@ T, p: Double): Option[List[(Double, B)]] = {
    // require((p gte h.min) && (p lte h.max) && h.buckets.nonEmpty)

    if(p lt h.min){
      upTo(h, h.min)
    }else if(p gt h.max){
      upTo(h, h.max)
    }else{
      (h: HistogramData[A,B]) match {
        case HistogramDataN(bs, mn, mx)=>
          val bs = h.buckets.toList

          (bs.head :: bs.zip(bs.drop(1)).takeWhile{ _._1._1 lte p }.map(_._2)).some

        case HistogramDataLTE1(bs, mn, mx)=> none
      }
    }
  }

  def bucketsFor(h: HistogramData[A,B] @@ T, p: Double): Option[((Double, B), (Double, B))] =
    upTo(h,p) flatMap {
      _.takeRight(2) match {
        case (a :: b :: Nil) => (a, b).some
        case _ => none
      }
    }

  /** sum of counts from -Inf to p **/
  def cumsum(h: HistogramData[A,B] @@ T, p: Double): Option[Double] = {
    @annotation.tailrec
    def loop(bs: List[(Double, Double)], total: Double): Double = bs match {
      case (x0, y0) :: (x1, y1) :: Nil =>
        val y = y0 + (y1 - y0) / (x1 - x0) * (p - x0)

        total + (y0 / 2) + ((y0 + y) / 2 * (p - x0) / (x1 - x0))
      case (b, count) :: x :: rest =>
        loop(x :: rest, total + count)

      case _ => total
    }

    if(p gte h.max){
      h.size.toDouble.some
    }else if(p lt h.min){
      0.0.some
    }else{
      val bucketsUpToP = upTo(h, p).map{ _.map{ case (k, v) => (k, implicitly[HistogramValue[B]].count(v).toDouble) }.toList }

      bucketsUpToP map (bs => loop(bs, bs.head._2 / 2))
    }
  }

  /** find quantile using binary search **/
  def quantile(h: HistogramData[A,B] @@ T, q0: Double, tol: Double = 0.001): Option[Double] = {
    require((q0 gte 0.0) && (q0 lte 1.0))

    (h: HistogramData[A,B]) match {
      case HistogramDataN(_, _, _) =>
        if(q0 === 1.0){
          h.max.some
        }else if(q0 === 0.0){
          h.min.some
        }else{
          val total = h.size

          @annotation.tailrec
          def loop(lb: Double, ub: Double): Double = {
            val x = lb + (ub - lb) / 2

            val q = cumsum(h, x).get / total


            if((((ub - lb) / lb) lt 0.001) || (math.abs(q - q0) lte tol)){
              x
            }else if(q gt q0){
              loop(lb, x)
            }else{
              loop(x, ub)
            }
          }

          loop(h.min, h.max).some
        }
      case HistogramDataLTE1(_, _, _) => none
    }
  }

  def quantiles(h: HistogramData[A,B] @@ T, qs: Seq[Double], tol: Double = 0.001): Seq[(Double, Option[Double])] =
    qs.map(q => (q, quantile(h, q, tol)))

}

abstract class HistogramWithTarget[A, Y, T](override val maxBuckets: Int)(implicit mon: Monoid[(Long, Y)], hv: HistogramValue[(Long, Y)], hp: HistogramPoint[(A, Y), (Long, Y)], ave: TargetAverage[Y]) extends Histogram[(A, Y), (Long, Y), T](maxBuckets)(mon, hv, hp){
  def averageTarget(h: HistogramData[(A, Y), (Long, Y)] @@ T, p: Double): Option[Y] = bucketsFor(h, p) map {
    case ((x1, y1), (x2, y2)) => ave.average(y1._2, y2._2, 1 - (p - x1) / (x2 - x1))
  }
}

trait HistogramFunctions{

  def insertN[A, B, T](h: HistogramData[A,B] @@ T, a: A, n: Int)
                      (implicit hv: HistogramValue[B], mon: Monoid[B], hp: HistogramPoint[A, B], hst: Histogram[A, B, T]): HistogramData[A,B] @@ T =
    hst.insertN(h, a, n)

  def insert[A, B, T](h: HistogramData[A,B] @@ T, as: A*)
                     (implicit hv: HistogramValue[B], mon: Monoid[B], hp: HistogramPoint[A, B], hst: Histogram[A, B, T]): HistogramData[A,B] @@ T =
    hst.insert(h, as)

  def merge[A, B, T](h1: HistogramData[A,B] @@ T, h2: HistogramData[A,B] @@ T)
                    (implicit hv: HistogramValue[B], mon: Monoid[B], hp: HistogramPoint[A, B], hst: Histogram[A, B, T]): HistogramData[A,B] @@ T =
    hst.merge(h1, h2)

  def cumsum[A, B, T](h: HistogramData[A,B] @@ T, p: Double)
                     (implicit hv: HistogramValue[B], mon: Monoid[B], hp: HistogramPoint[A, B], hst: Histogram[A, B, T]): Option[Double] =
    hst.cumsum(h, p)

  def quantile[A,B,T](h: HistogramData[A,B] @@ T, q0: Double, tol: Double = 0.001)
                     (implicit hv: HistogramValue[B], mon: Monoid[B], hp: HistogramPoint[A, B], hst: Histogram[A, B, T]): Option[Double] =
    hst.quantile(h, q0, tol)

  def quantiles[A, B, T](h: HistogramData[A,B] @@ T, qs: Seq[Double], tol: Double = 0.001)
                        (implicit hv: HistogramValue[B], mon: Monoid[B], hp: HistogramPoint[A, B], hst: Histogram[A, B, T]): Seq[(Double, Option[Double])] =
    hst.quantiles(h, qs, tol)
}

object hstgm extends HistogramFunctions {

  implicit def simpleHistogramValue[A : Numeric : Monoid] = new HistogramValue[A]{
    def count(a: A) = implicitly[Numeric[A]].toLong(a)
  }
  implicit def histogramWithTargetValue[Y : Monoid] = new HistogramValue[(Long, Y)]{
    def count(a: (Long, Y)) = a._1
  }


  implicit def numericTargetAverage[Y : Numeric : Monoid] = new TargetAverage[Y]{
    def weighted(y: Y, w: Double): Y =
      implicitly[Numeric[Y]].times(y, implicitly[Numeric[Y]].fromDouble(w))
  }
  implicit def momentsTargetAverage = new TargetAverage[Moments]{
    def weighted(y: Moments, w: Double) = y.copy(n = math.round(w * y.n).toLong)

    override def average(y1: Moments, y2: Moments, w: Double): Moments =
      super.average(y1, y2, w).copy(n = y1.n + y2.n)

  }

  implicit def simpleHistogramPoint[A : Numeric] = new HistogramPoint[A, Long]{
    def point(a: A) = implicitly[Numeric[A]].toDouble(a)

    def value(a: A) = 1L
  }

  implicit def simpleHistogramWithTargetPoint[A : Numeric, Y : Monoid] = new HistogramPoint[(A, Y), (Long, Y)]{
    def point(ay: (A, Y)) = implicitly[Numeric[A]].toDouble(ay._1)

    def value(ay: (A, Y)) = (1L, ay._2)
  }

  def simpleHistogram[A : Numeric, T](n: Int, g: ((Double,Long),(Double,Long)) => Double = gapfn.distance)(implicit hp: HistogramPoint[A,Long]) = new Histogram[A, Long, T](n){
    def gapSize(x: (Double, Long), y: (Double, Long)): Double = g(x,y)
  }

  def simpleHistogramWithTarget[A, Y, T](n: Int, g: ((Double,Long),(Double,Long)) => Double = gapfn.distance)(implicit num: Numeric[A], monY: Monoid[Y], hp: HistogramPoint[(A,Y), (Long, Y)], ave: TargetAverage[Y]) = new HistogramWithTarget[A, Y, T](n){
    def gapSize(x: (Double, Long), y: (Double, Long)): Double = g(x, y)
  }

  object gapfn{
    val distance: ((Double,Long),(Double,Long)) => Double = (x, y) => y._1 - x._1

    val countWeightedDistance: ((Double,Long),(Double,Long)) => Double = (x, y) => distance(x,y) * math.log(1 + x._2 + y._2)
  }
}

