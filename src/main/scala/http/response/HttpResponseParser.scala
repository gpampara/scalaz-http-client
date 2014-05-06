package http
package response

import scalaz._
import atto._, Atto._

object HttpResponseParser {

  lazy val cr: Parser[Char] =
    char(0x0D)

  lazy val lf: Parser[Char] =
    char(0x0A)

  lazy val eol: Parser[Unit] =
    (cr ~ lf | cr | lf) map (_ => ())

  lazy val dot =
    char('.')

  lazy val zeroD: Parser[Option[Digit]] =
    char('0') map (Digit.digitFromChar(_))

  lazy val oneD: Parser[Option[Digit]] =
    char('1') map (Digit.digitFromChar(_))

  import scalaz.syntax.apply._
  import scalaz.syntax.std.list._
  import scalaz.std.option._
  import scalaz.std.list._
  import scalaz.syntax.std.string._

  lazy val version: Parser[Option[Version]] =
    for {
      _ <- string("HTTP/")
      major <- oneD <~ dot
      minor <- (oneD | zeroD)
    } yield (major |@| minor)(Version.version(_, _))

  lazy val httpCode =
    int.map(Status.fromInt(_)) as "httpCode"

  lazy val alphaText: Parser[List[Char]] =
    many(letterOrDigit | spaceChar | char('-'))

  lazy val statusLine =
    for {
      v <- version <~ spaceChar
      c <- httpCode <~ spaceChar
      t <- alphaText <~ eol
    } yield (v |@| c)(StatusLine.statusLine(_, _, t))

  lazy val printableChar: Parser[Char] =
    elem(c => c >= 32 && c < 127)

  lazy val header: Parser[Option[(ResponseHeader, NonEmptyList[Char])]] =
    for {
      key <- alphaText <~ string(": ")
      value <- many1(printableChar) <~ eol
    } yield (ResponseHeader.fromString(key.mkString) |@| value.mkString.charsNel)((_, _))

  lazy val httpHeader =
    for {
      line <- statusLine
      headers <- many1(header)
    } yield {
      import Scalaz._
      (line |@| headers.sequence)(HttpResponse.response[Stream](_, _, Stream.empty))
    }

  lazy val response =
    for {
      h <- httpHeader
      body <- opt(many(printableChar) <~ eol)
    } yield h.map(_ >> body.map(l => l.map(_.toByte).toStream).getOrElse(Stream.empty))
}
