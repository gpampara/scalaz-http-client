package http
package response

import scalaz._
import scalaz.syntax.std.string._
import scalaz.syntax.foldable._

sealed trait HttpResponse[F[_]] {
  val line: StatusLine

  val headers: List[(ResponseHeader, NonEmptyList[Char])]

  val body: F[Byte]

  def >>(b: F[Byte]) = HttpResponse.response(line, headers, b)

  def versionMajor = line.version.major

  def verionMino = line.version.minor

  def status = line.status

  // This function could benefit from caching of the headers in a mp perhaps?
  def contentType(f : NonEmptyList[Char] => Boolean) =
    headers find { case (k, v) => k == ContentType } map (x => f(x._2)) getOrElse false

  def cotentTypeEquals(s: String) =
    contentType(_.list.mkString == s)

  def bodyLength(implicit f: Foldable[F]) =
    f.count(body)

  def bodyString(implicit f: Foldable[F]) =
    body.foldLeft(new java.lang.StringBuilder)((builder, a) => builder append a.toChar).toString

}

object HttpResponse {
  def response[F[_]](l: StatusLine, h: List[(ResponseHeader, NonEmptyList[Char])], b: F[Byte]) =
    new HttpResponse[F] {
      val line = l
      val headers = h
      val body = b
    }
}
