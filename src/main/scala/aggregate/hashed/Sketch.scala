/*
package scalaton.aggregate.hashed

import scalaz._
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
with MakesSingletonM[A,B,Int,T,R,F]
with Sized[F]
with Equal[F]

object sketch
extends CountMinSketchInstances
with HashedCollectionFunctions
with MakesSingletonMFunctions
with SetLikeFunctions
with MapLikeFunctions
with SizedFunctions

*/
