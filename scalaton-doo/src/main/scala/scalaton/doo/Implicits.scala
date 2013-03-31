package scalaton.doo

import scalaton.util._
import scalaton.util.hashing._
import scalaton.util.hashing32._

import com.nicta.scoobi.Scoobi._

import scalaz.{DList => _, _}
import Scalaz._

trait EnrichedDList[A]{
  val dl: DList[A]
}

trait ImplicitConversions{
  implicit def enrichDListWithSample[A : Manifest : WireFormat](x: DList[A])(implicit hashable: Hashable[A,Bits32]) =
    new EnrichedDList[A]{
      val dl = x

      def sample(rate: Double, seed: Int = 0) = sampling.sample(dl, rate, seed)
    }

  implicit def enrichDListWithSampleBy[A : Manifest : WireFormat, B : Manifest : WireFormat](x: DList[(A,B)])(implicit hashable: Hashable[A,Bits32]) =
    new EnrichedDList[(A,B)]{
      val dl = x

      def sampleBy(rate: Double, seed: Int = 0) = sampling.sampleBy(dl, rate, seed)
    }

  implicit def enrichDListWithSample[A : Manifest : WireFormat](x: DList[A]) =
    new EnrichedDList[A]{
      val dl = x

      def limit(n: Int = 0) = sampling.limit(dl, n)
    }

}

object implicits extends ImplicitConversions
