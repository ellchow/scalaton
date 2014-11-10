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

  implicit def wrapInAJsonEncodeable[A : EncodeJson](a: A) = JsonEncodeable(a)
  implicit def wrapInFAJsonEncodeable[F[_], A](fa: F[A])(implicit e: EncodeJson[F[A]]) = JsonEncodeable(fa)
  implicit val JsonEncodeableEncodeJson = EncodeJson((w: JsonEncodeable) => w.json)

  private[scalaton] trait JsonEncodeable {
    type A
    val value: A
    def json: Json
  }
  object JsonEncodeable {
    def apply[AA : EncodeJson](v: AA): JsonEncodeable = new JsonEncodeable {
      type A = AA
      val value = v
      lazy val json = v.asJson
    }
  }
  sealed trait BuildWithJson {
    def w: JsonEncodeable
    def build[A : DecodeJson]: (String, CursorHistory) \/ A = w.json.as[A].toDisjunction
  }
  object BuildWithJson {
    def apply(m: Map[String,JsonEncodeable] = Map.empty) = MapBuildWithJson(m)
    def apply(w: JsonEncodeable) = LeafBuildWithJson(w)
  }
  case class MapBuildWithJson(m: Map[String,JsonEncodeable]) extends BuildWithJson {
    def + [A <% JsonEncodeable](kv: (String, A)) = MapBuildWithJson(m + (kv._1 -> kv._2))
    def w = JsonEncodeable(m)
  }
  case class LeafBuildWithJson(w: JsonEncodeable) extends BuildWithJson
}
