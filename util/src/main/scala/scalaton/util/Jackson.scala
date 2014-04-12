package scalaton.util

import scala.reflect._
import scala.reflect.runtime.universe._
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala._
import com.fasterxml.jackson.module.scala.deser._
import java.lang.reflect.{ Type, ParameterizedType }
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.core.`type`.TypeReference
import scalaz._, Scalaz._

object Jackson {
  object Implicits {
    implicit def defaultObjectMapper = {
      val m = new ObjectMapper
      m.registerModule(DefaultScalaModule)
      m
    }
  }

  implicit class JacksonJsonOps(val x: Any) {
    def toJson(implicit mapper: ObjectMapper) = mapper.writeValueAsString(x)
  }

  implicit class JacksonStringOps(val x: String) {
    def fromJson[T : Manifest](implicit mapper: ObjectMapper): T =
      mapper.readValue(x, typeReference[T])

    private [this] def typeReference[T : Manifest] = new TypeReference[T] {
      override def getType = typeFromManifest(manifest[T])
    }

    private [this] def typeFromManifest(m: Manifest[_]): Type = {
      if (m.typeArguments.isEmpty) {
        m.runtimeClass
      } else {
        new ParameterizedType {
          def getRawType = m.runtimeClass
          def getActualTypeArguments = m.typeArguments.map(typeFromManifest).toArray
          def getOwnerType = null
        }
      }
    }
  }

  def fieldFromPath(x: Any, path: List[String])(implicit mapper: ObjectMapper) = {
    val any = x.toJson.decode[Any]
    def loop(any: Any, remaining: List[String]): Throwable \/ Any = (any, remaining) match {
      case (_, Nil) => any.right
      case (m: Map[_, _], p :: ps) =>
        m.collect{ case (k: String, v: Any) => (k, v) }
          .get(p).map(x => loop(x, ps))
          .getOrElse(new NoSuchElementException(p).left)
      case _ => new IllegalArgumentException(s"invalid path ${path.mkString}").left
    }
    loop(any, path)
  }


}
