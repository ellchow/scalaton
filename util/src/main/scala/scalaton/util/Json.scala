package scalaton.util

import argonaut._, Argonaut._

object Json {

  implicit def GenericMapEncodeJson[K: EncodeJson, V: EncodeJson]: EncodeJson[Map[K,V]] =
    EncodeJson((x: Map[K,V]) => x.toList.asJson)

  implicit def GenericMapDecodeJson[K: DecodeJson, V: DecodeJson]: DecodeJson[Map[K,V]] =
    implicitly[DecodeJson[List[(K,V)]]].map(_.toMap)

  implicit def ByteArrayEncodeJson: EncodeJson[Array[Byte]] =
    jencode1L((x: Array[Byte]) => new String(x))("bytes")

  def ByteArrayDecodeJson[A](f: Array[Byte] => A): DecodeJson[A] =
    jdecode1L((x: String) => x.getBytes)("bytes").map(f)

}
