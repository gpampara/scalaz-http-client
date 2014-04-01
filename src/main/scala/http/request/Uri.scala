package http
package request

import scalaz._

sealed trait Uri {
  val path: NonEmptyList[Char]

  val queryString: Option[NonEmptyList[Char]]
}

object Uri {

  def uri(p: NonEmptyList[Char], q: Option[NonEmptyList[Char]]) =
    new Uri {
      val path = p
      val queryString = q
    }

  implicit object UriShow extends Show[Uri] {
    override def shows(a: Uri) =
      a.path.list.mkString + a.queryString.map(x => "?" + x.list.mkString).getOrElse("")
  }
}
