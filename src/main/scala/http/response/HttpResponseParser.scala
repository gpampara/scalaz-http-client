package http
package response

import scalaz._

object HttpResponseParser {
  import Scalaz._
  import scala.util.parsing.combinator._

  final case class ParserError(msg: String, pos: String)

  object Parser extends RegexParsers {
    override val skipWhitespace = false
    val eol = """\r\n""".r
    val digit = """1|0""".r
    val version = digit ~ "." ~ digit ^^ {
      case maj ~ _ ~ min =>
        (Digit.digitFromChar(maj.charAt(0)) |@| Digit.digitFromChar(min.charAt(0))) {
          Version.version(_, _)
        }
    }
    val code: Parser[Option[Status]] = repN(3, """\d""".r) ^^ { l => Status.fromInt(l.foldLeft("")(_ + _).toInt) }
    val alphaSpace: Parser[String] = """[a-zA-Z0-9 -]+""".r
    val statusLine: Parser[Option[StatusLine]] = "HTTP/" ~> version ~ " " ~ code ~ " " ~ alphaSpace ~ eol ^^ {
      case v ~ _ ~ c ~ _ ~ a ~ _ =>
        for {
          vv <- v
          cc <- c
        } yield StatusLine.statusLine(vv, cc, a.toList)
    }

    val header: Parser[Option[(ResponseHeader, NonEmptyList[Char])]] = alphaSpace ~ ": " ~ """.+""".r <~ eol ^^ {
      case a ~ _ ~ b => for {
        header <- ResponseHeader.fromString(a)
        message <- b.charsNel
      } yield { println("header => " + header + ": " + message); (header, message) }
    }

    val httpHeader: Parser[Option[HttpResponse[Stream]]] = statusLine ~ rep(header) ~ eol ^^ {
      case status ~ headers ~ _ =>
        val hdrs: List[(ResponseHeader, NonEmptyList[Char])] = headers.flatMap(_.toList)
        status map { x => HttpResponse.response[Stream](x, hdrs, Stream.empty) }
    }

    def apply(in: String): ParserError \/ HttpResponse[Stream] = parse(httpHeader, in) match {
      case Success(result, next) => {
        val rest = next.source.toString.drop(next.offset)
        val r: Option[HttpResponse[Stream]] = result
        r.map(x => {
          val length = x.headers find { case (k, v) => k.asString == ContentLength.asString } map (_._2) flatMap (_.list.mkString.parseInt.toOption) getOrElse 0
          x >> rest.toStream.map(_.toByte).take(length)
        }).toRightDisjunction(ParserError("Error in parsing HTTP response", ""))
      }
      case NoSuccess(msg, next) => \/.left(ParserError(msg, next.pos.longString))
    }
  }
}
