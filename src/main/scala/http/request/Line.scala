package http
package request

import scalaz._

sealed trait Line {
  val method: Method

  val uri: Uri

  val version: Version
}

object Line {
  import syntax.show._

  def line(m: Method, u: Uri, v: Version) =
    new Line {
      val method = m
      val uri = u
      val version = v
    }

  implicit object LineShow extends Show[Line] {
    override def shows(a: Line) = {
      a.method.asString + " " + a.uri.shows + " " + a.version.shows
    }
  }
}
