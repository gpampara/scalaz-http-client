package http
package response

sealed trait StatusLine {
  val version: Version

  val status: Status

  val reasonPhrase: List[Char]
}

object StatusLine {
  def statusLine(v: Version, s: Status, p: List[Char]) = new StatusLine {
    val version = v
    val status = s
    val reasonPhrase = p
  }
}
