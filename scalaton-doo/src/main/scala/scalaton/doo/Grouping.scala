package scalaton.doo

import com.nicta.scoobi.Scoobi._

import scalaz.{DList => _, _}
import Scalaz._

trait GroupingFunctions{
  def secondarySort[K : Grouping : Order, S: Order] = new Grouping[(K, S)] {
    override def partition(key: (K, S), howManyReducers: Int) =
      implicitly[Grouping[K]].partition(key._1, howManyReducers)

    override def sortCompare(a: (K, S), b: (K, S)) = (a ?|? b)

    override def groupCompare(a: (K, S), b: (K, S)) = (a._1 ?|? b._1)

  }
}

object grouping extends GroupingFunctions
