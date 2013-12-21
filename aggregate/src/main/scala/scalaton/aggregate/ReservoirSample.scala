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

package scalaton.aggregate

import collection.mutable

import scalaz._
import Scalaz._

trait ReservoirSampleModule {

  case class Reservoir[A] private[aggregate] (val size: Int, val samples: Vector[A], inserted: Int) {
    require(size gt 0)

    def insert(a: A) = {
      if (inserted < size)
        this.copy(samples = samples :+ a,
          inserted = inserted + 1)
      else if (scala.util.Random.nextInt(inserted) < size)
        this.copy(samples = samples.updated(scala.util.Random.nextInt(size), a),
          inserted = inserted + 1)
      else
        this.copy(inserted = inserted + 1)
    }

  }

  object Reservoir{
    def empty[A](size: Int) = Reservoir[A](size, Vector.empty, 0)

    def fromData[A](size: Int, as: Iterable[A]) = as.foldLeft(empty[A](size))((r, a) => r.insert(a))
  }
}


object rsmpl extends ReservoirSampleModule
