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

package scalaton.doo

import com.nicta.scoobi.Scoobi._

import scalaz.{DList => _, _}
import Scalaz._

import org.slf4j.LoggerFactory

trait HelperFunctions {

  def parallelFold[A : Manifest : WireFormat, B : Manifest : WireFormat](dl: DList[A], init: B)(f: (B, A) => B) = {
    def foldFun = new DoFn[A, B]{
      private var b = init

      def setup() {}

      def process(a: A, emitter: Emitter[B]){
        b = f(b, a)
      }

      def cleanup(emitter: Emitter[B]) {
        emitter emit b
      }

    }

    dl parallelDo foldFun
  }

  def parallelFoldMonoid[A : Manifest : WireFormat, B : Manifest : WireFormat : Monoid](dl: DList[A])(f: (B, A) => B) =
    parallelFold(dl, implicitly[Monoid[B]].zero)(f)

  def cacheDList[A : Manifest : WireFormat : AvroSchema](dl: DList[A], path: String, overwrite: Boolean = false)(implicit sconf: ScoobiConfiguration): DList[A] = {
    val logger = LoggerFactory.getLogger("cacheDList")

    if(overwrite || !hdfs.exists(path) || !hdfs.isComplete(path)){
      logger.info("computing and writing output to $path%s")
      persist(toAvroFile(dl, path,overwrite=true))
    }else{
      logger.info("reading cached result from $path%s")
      fromAvroFile(path)
    }
  }

}


object helpers extends HelperFunctions
