package scalaton.aggregate.hashed

import scala.language.higherKinds
import scala.language.postfixOps

import scalaz.{BloomFilter => _, _}
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


