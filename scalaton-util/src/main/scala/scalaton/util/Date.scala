package scalaton.util

import com.github.nscala_time.time.Imports._

import scalaz._
import Scalaz._

trait DateInstances{
  implicit val localDateInstance = new Enum[LocalDate] {
    def succ(a: LocalDate): LocalDate = a + 1.day

    def pred(a: LocalDate): LocalDate = a - 1.day

    def order(a: LocalDate, b: LocalDate) = a.compareTo(b) match {
      case -1 => Ordering.LT
      case 0 => Ordering.EQ
      case 1 => Ordering.GT
    }
  }
}

object date
extends DateInstances
