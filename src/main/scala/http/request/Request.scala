package http
package request

import scalaz._
import scalaz.Id._
import scalaz.Kleisli._
import scalaz.concurrent._
import scalaz.syntax.std.string._

sealed trait HttpRequest[F[_]] {
  val line: Line

  val headers: List[(RequestHeader, NonEmptyList[Char])]

  val body: F[Byte]

  //def >>(b: F[Byte]) = HttpResponse.response(line, headers, b)
}

object HttpRequest {
  implicit object HttpRequestShow extends Show[HttpRequest[Stream]] {
    override def shows(a: HttpRequest[Stream]): String = {
      val b = new java.lang.StringBuilder
      b.append(Show[Line].shows(a.line)).append("\r\n")
      a.headers.foreach(x => b.append(x._1.asString + ": " + x._2.list.mkString).append("\r\n"))
      b.append("\r\n")
      b.append(a.body.mkString)
      b.toString
    }
  }

  // def request(l: Line, hdrs: List[(RequestHeader, NonEmptyList[Char])], content: Stream[Byte]) =
  //   new HttpRequest[Stream] {
  //     val line = l
  //     val headers = hdrs
  //     val body = content
  //   }

  // Should this be for _all_ requests? the Host header???
  def request(l: Line, h: List[(RequestHeader, NonEmptyList[Char])], c: Stream[Byte]): Kleisli[Task, java.net.InetSocketAddress, HttpRequest[Stream]] =
    kleisli(addr => {
      Task.now(new HttpRequest[Stream] {
        val line = l
        val headers = h
        val body = c
      })
    })

}
