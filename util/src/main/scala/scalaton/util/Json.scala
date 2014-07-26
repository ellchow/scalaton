package scalaton.util

import argonaut._, Argonaut._
import scalaz._, Scalaz._

object Json {

  implicit def genericMapEncodeJson[K: EncodeJson, V: EncodeJson]: EncodeJson[Map[K,V]] =
    EncodeJson((x: Map[K,V]) => x.toList.asJson)

  implicit def genericMapDecodeJson[K: DecodeJson, V: DecodeJson]: DecodeJson[Map[K,V]] =
    implicitly[DecodeJson[List[(K,V)]]].map(_.toMap)

  implicit val ByteEncodeJson = EncodeJson((x: Byte) => x.toInt.asJson)
  implicit val ByteDecodeJson = implicitly[DecodeJson[Int]].map(_.toByte)

  implicit def byteArrayEncodeJson: EncodeJson[Array[Byte]] =
    jencode1L((x: Array[Byte]) => x.toList.asJson)("bytes")

  def byteArrayDecodeJson[A](f: Array[Byte] => A): DecodeJson[A] =
    jdecode1L((x: List[Byte]) => x.toArray)("bytes").map(f)

  def makeDateTimeCodecJson(name: String): CodecJson[org.joda.time.DateTime] = CodecJson(
    dt => ("type" := name) ->: ("timestamp" := dt.getMillis) ->: jEmptyObject,
    c => for {
      dtString <- (c --\ "timestamp").as[Long]
      t <- (c --\ "type").as[String]
      _ <- if (t == name) DecodeResult.ok(()) else DecodeResult.fail(name, c.history)
      res <- \/.fromTryCatch(new org.joda.time.DateTime(dtString)) match {
        case \/-(a) => DecodeResult.ok(a)
        case -\/(err) => DecodeResult.fail(name, c.history)
      }
    } yield res
  )

  implicit val DateTimeCodecJson: CodecJson[org.joda.time.DateTime] = makeDateTimeCodecJson("datetime")

  implicit val localDateCodecJson: CodecJson[org.joda.time.LocalDate] = {
    val dtc = makeDateTimeCodecJson("localdate")
    val lt = new org.joda.time.LocalTime(0,0,0,0)
    CodecJson(ld => ld.toDateTime(lt).asJson(dtc), c => dtc.decode(c).map(_.toLocalDate))
  }
  implicit val LocalDateTimeCodecJson: CodecJson[org.joda.time.LocalDateTime] = {
    val dtc = makeDateTimeCodecJson("localdatetime")
    CodecJson(ldt => ldt.toDateTime.asJson(dtc), c => dtc.decode(c).map(_.toLocalDateTime))
  }

  implicit def wrapInAWEJson[A : EncodeJson](a: A) = WEJson(a)
  implicit def wrapInFAWEJson[F[_], A](fa: F[A])(implicit e: EncodeJson[F[A]]) = WEJson(fa)
  implicit val weJsonEncodeJson = EncodeJson((w: WEJson) => w.json)

  private[scalaton] trait WEJson {
    type A
    val value: A
    def json: Json
  }
  object WEJson {
    def apply[AA : EncodeJson](v: AA): WEJson = new WEJson {
      type A = AA
      val value = v
      lazy val json = v.asJson
    }
  }
  sealed trait BuildWithJson {
    def w: WEJson
    def build[A : DecodeJson]: (String, CursorHistory) \/ A = w.json.as[A].toDisjunction
  }
  object BuildWithJson {
    def apply(m: Map[String,WEJson] = Map.empty) = MapBuildWithJson(m)
    def apply(w: WEJson) = LeafBuildWithJson(w)
  }
  case class MapBuildWithJson(m: Map[String,WEJson]) extends BuildWithJson {
    def + [A <% WEJson](kv: (String, A)) = MapBuildWithJson(m + (kv._1 -> kv._2))
    def w = WEJson(m)
  }
  case class LeafBuildWithJson(w: WEJson) extends BuildWithJson
}
