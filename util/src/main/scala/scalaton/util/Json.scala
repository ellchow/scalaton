package scalaton.util

import argonaut._, Argonaut._
import scalaz._, Scalaz._

object Json {

  implicit def genericMapEncodeJson[K: EncodeJson, V: EncodeJson]: EncodeJson[Map[K,V]] =
    EncodeJson((x: Map[K,V]) => x.toList.asJson)

  implicit def genericMapDecodeJson[K: DecodeJson, V: DecodeJson]: DecodeJson[Map[K,V]] =
    implicitly[DecodeJson[List[(K,V)]]].map(_.toMap)

  implicit def byteArrayEncodeJson: EncodeJson[Array[Byte]] =
    jencode1L((x: Array[Byte]) => new String(x))("bytes")

  def byteArrayDecodeJson[A](f: Array[Byte] => A): DecodeJson[A] =
    jdecode1L((x: String) => x.getBytes)("bytes").map(f)

  implicit def wrapInAWEJson[A : EncodeJson](a: A) = WEJson(a)
  implicit def wrapInFAWEJson[F[_], A](fa: F[A])(implicit e: EncodeJson[F[A]]) = WEJson(fa)
  implicit val weJsonEncodeJson = EncodeJson((w: WEJson) => w.json)

  private[util]trait WEJson {
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
