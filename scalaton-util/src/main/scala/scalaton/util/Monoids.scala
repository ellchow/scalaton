package scalaton.util

import scalaz.{Tags => ZTags,_}
import Scalaz._

object monoids{

  implicit val maxIntMonoid: Monoid[Int @@ ZTags.Max] = Monoid instance ((l, r) => Tag(l max r), ZTags.Max(Int.MinValue))
  implicit val minIntMonoid: Monoid[Int @@ ZTags.Min] = Monoid instance ((l, r) => Tag(l min r), ZTags.Min(Int.MaxValue))

  implicit val maxDoubleMonoid: Monoid[Double @@ ZTags.Max] = Monoid instance ((l, r) => Tag(l max r), ZTags.Max(Double.MinValue))
  implicit val minDoubleMonoid: Monoid[Double @@ ZTags.Min] = Monoid instance ((l, r) => Tag(l min r), ZTags.Min(Double.MaxValue))

  implicit val maxLongMonoid: Monoid[Long @@ ZTags.Max] = Monoid instance ((l, r) => Tag(l max r), ZTags.Max(Long.MinValue))
  implicit val minLongMonoid: Monoid[Long @@ ZTags.Min] = Monoid instance ((l, r) => Tag(l min r), ZTags.Min(Long.MaxValue))

  implicit val maxShortMonoid: Monoid[Short @@ ZTags.Max] = Monoid instance ((l, r) => Tag(l max r), ZTags.Max(Short.MinValue))
  implicit val minShortMonoid: Monoid[Short @@ ZTags.Min] = Monoid instance ((l, r) => Tag(l min r), ZTags.Min(Short.MaxValue))

  implicit val maxByteMonoid: Monoid[Byte @@ ZTags.Max] = Monoid instance ((l, r) => Tag(l max r), ZTags.Max(Byte.MinValue))
  implicit val minByteMonoid: Monoid[Byte @@ ZTags.Min] = Monoid instance ((l, r) => Tag(l min r), ZTags.Min(Byte.MaxValue))

}
