package scalaton.http

import scala.util.control.Exception._

import java.net.{URLEncoder, URLDecoder, URL}

import scalaz._
import Scalaz._
import Validation.fromTryCatch

object url {

  private val leadingSlashRegex = "^[/]?".r

  def encode(u: String, encoding: String = "UTF-8"): ValidationNEL[String, String] =
    fromTryCatch(URLEncoder.encode(u, encoding)) match {
      case Success(s) => s.successNel[String]
      case Failure(f) => f"failed to encode string: $u%s".failureNel[String]
    }

  def encodeIfPossible(u: String, encoding: String = "UTF-8"): String =
    encode(u) fold (_ => u, identity)

  def decode(u: String, encoding: String = "UTF-8"): ValidationNEL[String, String] =
    fromTryCatch(URLDecoder.decode(u, encoding)) match {
      case Success(s) => s.successNel[String]
      case Failure(f) => f"failed to decode string: $u%s".failureNel[String]
    }

  def decodeIfPossible(u: String, encoding: String = "UTF-8"): String =
    decode(u) fold (_ => u, identity)



  def constructQueryString(queryParams: Map[String,String], encoding: String = "UTF-8") =
    queryParams.map{ case (k, v) =>
                     encodeIfPossible(k, encoding) + "=" +
                     encodeIfPossible(v, encoding)}.mkString("&")

  def parseQueryString(queryString: String, encoding: String = "UTF-8") =
    catching(classOf[Exception]) opt {
      queryString.split("&") map { kv =>
        val pair = kv.split("=")
        if (pair.size == 2)
          (decodeIfPossible(pair(0)),decodeIfPossible(pair(1)))
        else
          throw new Exception
      } toMap
    }

  def apply(host: String, path: String = "", queryParams: Map[String,String] = Map.empty, port: Int = -1, protocol: String = "http") = {
    val file = leadingSlashRegex.replaceFirstIn(path, "/")
    val qry = (queryParams isEmpty) ? "" | ("?" + constructQueryString(queryParams))

    (new URL(protocol, host, port, file + qry)) toString
  }

  def unapply(u: String) = catching(classOf[Exception]) opt {
    val url = new URL(u)
    val qry = parseQueryString(url.getQuery ?? "").getOrElse(Map.empty)

    (url.getHost, url.getPath, qry: Map[String,String], url.getPort, url.getProtocol)
  }
}
