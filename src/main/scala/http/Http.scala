package http

import scalaz._
import scalaz.Kleisli._
import scalaz.concurrent._
import request._
import response._

object Http {
  import scalaz._, Scalaz._

  private def host(addr: java.net.InetSocketAddress) =
    Option(addr.getHostName + (if (addr.getPort != 80) ":" + addr.getPort else "")).flatMap(_.charsNel).cata(identity, "Unknown".unsafeCharsNel)

  private def getOrHead(addr: java.net.InetSocketAddress, path: String, queryParams: Option[String], method: Method) =
    (for {
      conn <- Transport.socket
      req <- HttpRequest.request(
        Line.line(method, Uri.uri(path.unsafeCharsNel, queryParams.flatMap(_.charsNel)), Version.version10),
        List((Host, host(addr)), (Connection, "close".unsafeCharsNel)),
        Stream.empty)
    } yield conn(req)).run(addr)

  def head(addr: java.net.InetSocketAddress, path: String, queryParams: Option[String]) =
    getOrHead(addr, path, queryParams, HEAD)

  def get(addr: java.net.InetSocketAddress, path: String, queryParams: Option[String]) =
    getOrHead(addr, path, queryParams, GET)

}

object Transport {
  import atto._, Atto._
  import scalaz.syntax.show._

  trait SocketProvider { // This is basically a Reader with params fixed
    def apply(request: HttpRequest[Stream]): String \/ Option[http.response.HttpResponse[Stream]]
  }

  val socket: Kleisli[Task, java.net.InetSocketAddress, SocketProvider] =
    kleisli(addr => Task.now(new SocketProvider {
      def apply(request: HttpRequest[Stream]) = {
        val socket = new java.net.Socket(addr.getHostName, addr.getPort)
        val writer = new java.io.PrintWriter(socket.getOutputStream)

        writer.write(request.shows)
        writer.flush()

        val s = scala.io.Source.fromInputStream(socket.getInputStream).takeWhile(_ != -1).mkString
        socket.close

        HttpResponseParser.response.parse(s).either
      }}))

  val secureSocketProvider = new SocketProvider {
    def apply(request: HttpRequest[Stream]) =
      ???
  }
}
