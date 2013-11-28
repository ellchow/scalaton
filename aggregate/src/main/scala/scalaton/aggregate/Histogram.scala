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

import scalaton.util.monoids._

/** implementation of online histogram building algorithm as described by http://jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf **/

trait HistogramModule{

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

  trait GapSizeFn{
    def apply(x: (Double, Long), y: (Double, Long)): Double
  }

  case object SimpleDistance extends GapSizeFn{
    def apply(x: (Double, Long), y: (Double, Long)): Double = y._1 - x._1
  }

  case object CountWeightedDistance extends GapSizeFn{
    def apply(x: (Double, Long), y: (Double, Long)): Double = (y._1 - x._1) * math.log(1 + x._2 + y._2)
  }

  /** wrapper class holding actual histogram buckets and values **/
  case class HistogramData[A, B](val buckets: TreeMap[Double, B], val min: Double, val max: Double, val n: Int,
                                 val gapSize: GapSizeFn = SimpleDistance)
                                (implicit monoidB: Monoid[B], hvB: HistogramValue[B], hpAB: HistogramPoint[A,B]){
    lazy val size = buckets.values.map(v => implicitly[HistogramValue[B]].count(v)).sum

    def isCompatibleWith(that: HistogramData[A,B]): Boolean =
      this.n === that.n

    /** insert a point into the histogram **/
    def insert(a: A): HistogramData[A,B] = {
      val p = hpAB.point(a)
      val v = hpAB.value(a)

      this.merge(HistogramData(TreeMap(p -> v), p, p, n, gapSize))
    }

    /** insert a point into the histogram n times **/
    def insertN(a: A, n: Int): HistogramData[A,B] = {
      require(n gte 0, "n must be > 0")

      (1 to n).foldLeft(this){ case (hh, _) => hh.insert(a) }
    }

    def insert(as: Iterable[A]): HistogramData[A,B] =
      as.foldLeft(this){ case (hh, a) => hh.insert(a) }

    def insert(as: A*): HistogramData[A,B] =
      this.insert(as)

    /** merge 2 histograms together **/
    def merge(that: HistogramData[A,B]): HistogramData[A,B] = {
      require(isCompatibleWith(that), "merging incompatible histograms")

      val unmergedBuckets = this.buckets |+| that.buckets

      @annotation.tailrec
      def loop(bs: TreeMap[Double, B]): TreeMap[Double, B] = {
        if(bs.size lte n)
          bs
        else{
          val (_, (k1, k2)) = ((Double.PositiveInfinity, (0.0, 0.0)) /: bs.view.zip(bs.view.drop(1))) {
            case (minSoFar, ((k1,v1), (k2,v2))) =>
              val w = gapSize((k1, hvB.count(v1)),
                              (k2, hvB.count(v2)))

              (w lt minSoFar._1) ? (w, (k1, k2)) | minSoFar
          }

          val v1 = bs(k1)
          val v2 = bs(k2)

          val k = (k1 * hvB.count(v1) + k2 * hvB.count(v2)) / (hvB.count(v1) + hvB.count(v2))
          val v = v1 |+| v2

          loop(bs - k1 - k2 + (k -> v))
        }
      }

      val mergedBuckets = loop(unmergedBuckets)
      val mergedMin = math.min(this.min, that.min)
      val mergedMax = math.max(this.max, that.max)

      HistogramData(mergedBuckets, mergedMin, mergedMax, n, gapSize)
    }

    /** get buckets (in ascending order) from histogram up to buckets that contain counts for the point **/
    def upTo(p: Double): Option[List[(Double, B)]] = {
      if(p lt this.min){
        none
      }else if(p gt this.max){
        none
      }else{
        if(this.size gt 1){
          val bs = this.buckets.toList

          (bs.head :: bs.zip(bs.drop(1)).takeWhile{ _._1._1 lte p }.map(_._2)).some
        }else{
          none
        }
      }
    }

    def bucketsFor(p: Double): Option[((Double, B), (Double, B))] =
      upTo(p) flatMap {
        _.takeRight(2) match {
          case (a :: b :: Nil) => (a, b).some
          case _ => none
        }
      }

    /** sum of counts from -Inf to p **/
    def cumsum(p: Double): Option[Double] = {
      @annotation.tailrec
      def loop(bs: List[(Double, Double)], total: Double): Double = bs match {
        case (x0, y0) :: (x1, y1) :: Nil =>
          val y = y0 + (y1 - y0) / (x1 - x0) * (p - x0)

          total + (y0 / 2) + ((y0 + y) / 2 * (p - x0) / (x1 - x0))
        case (b, count) :: x :: rest =>
          loop(x :: rest, total + count)

        case _ => total
      }

      if(p gte this.max){
        this.size.toDouble.some
      }else if(p lt this.min){
        0.0.some
      }else{
        val bucketsUpToP = upTo(p).map{ _.map{ case (k, v) => (k, hvB.count(v).toDouble) }.toList }

        bucketsUpToP map (bs => loop(bs, bs.head._2 / 2))
      }
    }

    /** find quantile using binary search **/
    def quantile(q0: Double, tol: Double = 0.001): Option[Double] = {
      require((q0 gte 0.0) && (q0 lte 1.0))

      if(this.size gt 1){
        if(q0 === 1.0){
          this.max.some
        }else if(q0 === 0.0){
          this.min.some
        }else{
          val total = this.size

          @annotation.tailrec
          def loop(lb: Double, ub: Double): Double = {
            val x = lb + (ub - lb) / 2

            val q = cumsum(x).get / total


            if((((ub - lb) / lb) lt 0.001) || (math.abs(q - q0) lte tol)){
              x
            }else if(q gt q0){
              loop(lb, x)
            }else{
              loop(x, ub)
            }
          }

          loop(this.min, this.max).some
        }
      }else{
        none
      }
    }

    def quantiles(qs: List[Double], tol: Double = 0.001): Option[List[(Double, Double)]] =
      qs.map(q => quantile(q, tol)).sequence.map(qs zip _)

  }

  object HistogramData{
    def empty[A,B](n: Int, gapSize: GapSizeFn = SimpleDistance)(implicit monoidB: Monoid[B], hvB: HistogramValue[B], hpAB: HistogramPoint[A,B]) =
      HistogramData[A,B](TreeMap.empty, Double.PositiveInfinity, Double.NegativeInfinity, n, gapSize)
  }

  implicit class HistogramWithTargetOps[A,Y](val h: HistogramData[(A,Y),(Long,Y)])
                                            (implicit monoid: Monoid[(Long, Y)], hv: HistogramValue[(Long, Y)], hp: HistogramPoint[(A, Y), (Long, Y)], ave: TargetAverage[Y]){
    def averageTarget(p: Double) = h.bucketsFor(p).map{
      case ((x1, y1), (x2, y2)) =>
        ave.average(y1._2, y2._2, 1 - (p - x1) / (x2 - x1))
    }
  }

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

  implicit def histogramSemigroup[A,B](implicit monoidB: Monoid[B], hvB: HistogramValue[B], hpAB: HistogramPoint[A,B]): Semigroup[HistogramData[A,B]] =
    new Semigroup[HistogramData[A,B]]{
      def append(h1: HistogramData[A,B], h2: => HistogramData[A,B]) = h1 merge h2
    }


}

object histogram extends HistogramModule
