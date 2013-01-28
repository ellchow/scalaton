/*
package scalaton.aggregate.hashed

import scalaz._
import Scalaz._


/** A container to extract a value from a container **/
trait Value[A,B]{
  def valueOf(a: A): B
}

object Value{
  /** identity **/
  implicit def anyValue[A] = new Value[A,A]{
    def valueOf(a: A): A = a
  }
}


*/
