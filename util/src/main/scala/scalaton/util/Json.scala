package scalaton.util

import argonaut._, Argonaut._

object Json {

  implicit def GenericMapEncodeJson[K,V](implicit ek: EncodeJson[K], ev: EncodeJson[V]): EncodeJson[Map[K, V]] =
    EncodeJson((x: Map[K,V]) => implicitly[EncodeJson[List[(K,V)]]].encode(x.toList))

  implicit def GenericMapDecodeJson[K,V](implicit ek: DecodeJson[K], ev: DecodeJson[V]): DecodeJson[Map[K, V]] =
    implicitly[DecodeJson[List[(K,V)]]].map(_.toMap)

}
