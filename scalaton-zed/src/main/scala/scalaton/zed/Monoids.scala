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

package scalaton.zed

import scalaz._
import Scalaz._


trait MonoidInstances{
  implicit val maxIntMonoid: Monoid[Int @@ Tags.Max] = Monoid instance ((l, r) => Tag(l max r), Tags.Max(Int.MinValue))
  implicit val minIntMonoid: Monoid[Int @@ Tags.Min] = Monoid instance ((l, r) => Tag(l min r), Tags.Min(Int.MaxValue))

  implicit val maxDoubleMonoid: Monoid[Double @@ Tags.Max] = Monoid instance ((l, r) => Tag(l max r), Tags.Max(Double.MinValue))
  implicit val minDoubleMonoid: Monoid[Double @@ Tags.Min] = Monoid instance ((l, r) => Tag(l min r), Tags.Min(Double.MaxValue))

  implicit val maxLongMonoid: Monoid[Long @@ Tags.Max] = Monoid instance ((l, r) => Tag(l max r), Tags.Max(Long.MinValue))
  implicit val minLongMonoid: Monoid[Long @@ Tags.Min] = Monoid instance ((l, r) => Tag(l min r), Tags.Min(Long.MaxValue))

  implicit val maxShortMonoid: Monoid[Short @@ Tags.Max] = Monoid instance ((l, r) => Tag(l max r), Tags.Max(Short.MinValue))
  implicit val minShortMonoid: Monoid[Short @@ Tags.Min] = Monoid instance ((l, r) => Tag(l min r), Tags.Min(Short.MaxValue))

  implicit val maxByteMonoid: Monoid[Byte @@ Tags.Max] = Monoid instance ((l, r) => Tag(l max r), Tags.Max(Byte.MinValue))
  implicit val minByteMonoid: Monoid[Byte @@ Tags.Min] = Monoid instance ((l, r) => Tag(l min r), Tags.Min(Byte.MaxValue))
}

object monoids
extends MonoidInstances

