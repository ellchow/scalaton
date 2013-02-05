package scalaton.util.http

import scala.util.control.Exception._
import scala.io.Source

import java.net.{URLEncoder, URLDecoder, URL, HttpURLConnection}

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

object request{

  def defaultHttpRequestSettings(s: HttpURLConnection){
    s.setConnectTimeout(5000)
    s.setReadTimeout(20000)
    s.setRequestMethod("GET")
    s.setRequestProperty("User-Agent", "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.4; en-US; rv:1.9.2.2) Gecko/20100316 Firefox/3.6.2")
  }

  def sendHttpRequest(url: String, numRetries:Int = 0, settings: HttpURLConnection => Unit = defaultHttpRequestSettings _): Option[(Int, io.BufferedSource)] = {
    val response = catching(classOf[Exception]).opt{
      val connection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
      settings(connection)

      connection.connect
      (connection.getResponseCode, Source.fromInputStream(connection.getInputStream))
    }

    response match {
      case Some(r) => response

      case _ if numRetries > 0 =>
        sendHttpRequest(url, numRetries - 1, settings)

      case _ => none
    }
  }
}
