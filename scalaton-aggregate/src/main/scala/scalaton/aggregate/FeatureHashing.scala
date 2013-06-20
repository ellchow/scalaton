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

import scala.annotation.tailrec

import scalaton.util._
import scalaton.util.hashing._

import scalaz._
import Scalaz._

trait FeatureHashingModule{

  def hashFeature[A,H1](feature: A, min: Int = 0, max: Int = Int.MaxValue, seed: Long = 0L)
                       (implicit h: Hashable[A,H1], hconv: HashCodeConverter[H1,Bits32]): (Int, Double) = {

    val hashed: Bits32 @@ HashCode = hconv.convert(hash(feature, seed)).head

    val hashedInRange = (hashed % (max - min + 1)) + min

    (hashedInRange, 1.0)
  }

  def hashFeatures[A,H1](features: Iterable[A], min: Int = 0, max: Int = Int.MaxValue, seed: Long = 0L)
                        (implicit h: Hashable[A,H1], hconv: HashCodeConverter[H1,Bits32]): Seq[(Int,Double)] =
    features.map(f => hashFeature(f, min, max, seed)(h,hconv)).toSeq

  def ngrams[A](features: Seq[A], n: Int = 2) = {
    @tailrec
    def loop(xs: Vector[Seq[A]], remaining: Seq[A]): Vector[Seq[A]]  =
      if(remaining.nonEmpty && remaining.size >= n)
        loop(xs :+ (remaining take n), remaining drop 1)
      else
        xs

    loop(Vector[Seq[A]](), features)
  }

  // def combinations[A,B](aFeatures: List[A], bFeatures: List[B]) =
    // (aFeatures |@| bFeatures) ((_, _))
}

object featurehash extends FeatureHashingModule
