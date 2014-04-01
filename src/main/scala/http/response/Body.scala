package http
package response

object Body {
  import scalaz._
  import Scalaz._

  def gunzip = (resp: HttpResponse[Stream]) => {
    val gis = new java.util.zip.GZIPInputStream(new java.io.ByteArrayInputStream(resp.body.take(resp.bodyLength).toArray))
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(gis, "UTF-8"))
    val builder = new java.lang.StringBuilder
    Stream.continually(reader.readLine).flatMap(x => if (x == null) Stream.empty else Stream(x)).foreach(builder.append(_))
    builder.toString
  }
}
