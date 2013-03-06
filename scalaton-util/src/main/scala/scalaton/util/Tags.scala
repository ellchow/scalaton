package scalaton.util

import scalaz._
import Scalaz._


object Tags{
  trait Vec
  def Vec[A](a: A): A @@ Vec = Tag(a)


}
