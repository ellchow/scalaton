package scalaton.util

import com.github.nscala_time.time.Imports._

import scalaz._
import Scalaz._

object date {

  implicit val localDateEnum = new Enum[LocalDate] {
    def succ(a: LocalDate): LocalDate = a + 1.day

    def pred(a: LocalDate): LocalDate = a - 1.day

    def order(a: LocalDate, b: LocalDate) = a.compareTo(b) match {
      case -1 => Ordering.LT
      case 0 => Ordering.EQ
      case 1 => Ordering.GT
    }
  }

}
