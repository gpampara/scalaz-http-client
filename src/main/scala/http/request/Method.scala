package http
package request

sealed trait Method {
  val asString: String
}

case object GET extends Method {
  val asString = "GET"
}
