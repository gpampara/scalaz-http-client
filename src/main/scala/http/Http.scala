package http

import scalaz._
import scalaz.Kleisli._
import scalaz.concurrent._
import request._
import response._

object Http {
  import RequestHeader._

  import scalaz.syntax.std.option._
  import scalaz.syntax.std.string._

  private def host(addr: java.net.InetSocketAddress) =
    Option(addr.getHostName + (if (addr.getPort != 80) ":" + addr.getPort else "")).flatMap(_.charsNel).cata(identity, "Unknown".unsafeCharsNel)

  import HttpResponseParser.ParserError

  // http.post("loginGet", params).map(_ match {
  //   case Http.SimpleHttpResult(code, content) if (code / 100 == 2) =>
  //     $d("Logged in to remote server")
  //     new Remote(http)
  //   case Http.SimpleHttpResult(410, _) => throw new RemoteLoginException("INVALID USER")
  //   case Http.SimpleHttpResult(401, _) => throw new RemoteLoginException("INVALID PASSWORD")
  //   case Http.SimpleHttpResult(code, error) => throw new RemoteHttpFailure(code, error)
  // })
  def login(username: String, password: String, f: HttpResponse[Stream] => Unit): Kleisli[Task, java.net.InetSocketAddress, Unit] =
    Kleisli.kleisli((addr: java.net.InetSocketAddress) =>
      (for {
        a <- HttpRequest.request(
          Line.line(GET, Uri.uri("/loginGet".unsafeCharsNel, Map("email" -> username, "pass" -> password, "noRedirect" -> "1").map(x => x._1 + "=" + x._2).mkString("&").charsNel), Version.version11),
          List((Host, host(addr)), (Connection, "close".unsafeCharsNel)),
          Stream.empty)
        b <- Transport.socket
      } yield b(a).foreach(f)).run(addr)) // save the cookie from the server in the sharedpreferences

  def withAuth(f: HttpResponse[Stream] => Unit, x: Kleisli[Task, java.net.InetSocketAddress, ParserError \/ HttpResponse[Stream]]) =//: Kleisli[Task, java.net.InetSocketAddress, ParserError \/ HttpResponse[Stream]] =
    kleisli((addr: java.net.InetSocketAddress) => {
      import scalaz.syntax.bind._
      x.run(addr).flatMap(_ match {
        case left @ -\/(e) =>
          Task.now(left)
        case right @ \/-(h) =>
          if (h.status == Found || h.status == Unauthorized) {
            (login("admin", "pass", f) >> x).run(addr) // do login (ignoring the output, because it's a really a side-effect) and then do x
          } else Task.now(right)
      })
    })

  def get(f: HttpResponse[Stream] => Unit)(addr: java.net.InetSocketAddress, path: String, queryParams: Option[String]) =
    withAuth(f, for {
      a <- Transport.socket
      b <- HttpRequest.request(
        Line.line(GET, Uri.uri(path.unsafeCharsNel, queryParams.flatMap(_.charsNel)), Version.version11),
        List((Host, host(addr)), (Connection, "close".unsafeCharsNel)),
        Stream.empty)
    } yield a(b)).run(addr)

  def post(params: Map[String, String]) =
    ???
}

object Transport {
  import scalaz.syntax.show._
  import HttpResponseParser._

  trait SocketProvider { // This is basically a Reader with params fixed
    def apply(request: HttpRequest[Stream]): ParserError \/ HttpResponse[Stream]
  }

  val socket: Kleisli[Task, java.net.InetSocketAddress, SocketProvider] =
    kleisli(addr => Task.now(new SocketProvider {
      def apply(request: HttpRequest[Stream]) = {
        val socket = new java.net.Socket(addr.getHostName, addr.getPort)
        val writer = new java.io.PrintWriter(socket.getOutputStream)

        writer.write(request.shows)
        writer.flush()

        val s = scala.io.Source.fromInputStream(socket.getInputStream).mkString
        socket.close

        Parser(s)
      }}))

  val secureSocketProvider = new SocketProvider {
    def apply(request: HttpRequest[Stream]) =
      ???
  }

}
