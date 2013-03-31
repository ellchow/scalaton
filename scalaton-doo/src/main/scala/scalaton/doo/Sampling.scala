package scalaton.doo

import scalaton.util._
import scalaton.util.hashing._
import scalaton.util.hashing32._

import com.nicta.scoobi.Scoobi._

import scalaz.{DList => _, _}
import Scalaz._

trait SamplingFunctions {

  def sample[A : Manifest : WireFormat](dl: DList[A], rate: Double, seed: Int = 0)(implicit hashable: Hashable[A,Bits32]) = {
    val n = (Int.MaxValue * rate) toInt

    dl.filter{ x => math.abs(hash[A,Bits32](x)) < n }
  }

  def sampleBy[A : Manifest : WireFormat, B : Manifest : WireFormat](dl: DList[(A,B)], rate: Double, seed: Int = 0)(implicit hashable: Hashable[A,Bits32]) = {
    val n = (Int.MaxValue * rate)

    dl.filter{ case (a, _) => math.abs(hash[A, Bits32](a)) < n }
  }

  def limit[A : Manifest : WireFormat](dl: DList[A], limit: Int) = {
    def limitFun = new DoFn[A, A] {
      private var count = limit;

      def setup() {}

      def process(input: A, emitter: Emitter[A]) {
        if (count > 0) {
          count = count - 1
          emitter.emit(input)
        }
      }

      def cleanup(emitter: Emitter[A]) {}
    }

    dl parallelDo limitFun
  }
}

object sampling extends SamplingFunctions
