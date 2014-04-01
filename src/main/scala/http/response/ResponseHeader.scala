package http
package response

sealed trait ResponseHeader {
  val asString: String
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.5">§</a>
  */
final case object AcceptRanges extends ResponseHeader {
  override val asString = "Accept-Ranges"
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.6">§</a>
  */
final case object Age extends ResponseHeader {
  override val asString = toString
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.19">§</a>
  */
final case object ETag extends ResponseHeader {
  override val asString = toString
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.30">§</a>
  */
final case object Location extends ResponseHeader {
  override val asString = toString
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.33">§</a>
  */
final case object ProxyAuthenticate extends ResponseHeader {
  override val asString = "Proxy-Authenticate"
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.37">§</a>
  */
final case object RetryAfter extends ResponseHeader {
  override val asString = "Retry-After"
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.38">§</a>
  */
final case object Server extends ResponseHeader {
  override val asString = toString
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.44">§</a>
  */
final case object Vary extends ResponseHeader {
  override val asString = toString
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.47">§</a>
  */
final case object WWWAuthenticate extends ResponseHeader {
  override val asString = "WWW-Authenticate"
}
private final case class Entity(eh: EntityHeader) extends ResponseHeader {
  override val asString = eh.asString
}
private final case class General(gh: GeneralHeader) extends ResponseHeader {
  override val asString = gh.asString
}

object ResponseHeader {
  implicit def entity2Response(eh: EntityHeader): ResponseHeader = Entity(eh)

  implicit def general2Response(eh: GeneralHeader): ResponseHeader = General(eh)

  def fromString(s: String): Option[ResponseHeader] =
    headers.find { case (n, h) => n == s }.map(_._2) orElse
      GeneralHeader.from(s).map(General(_)) orElse
      EntityHeader.from(s).map(Entity(_))

  val headers = List(("accept-ranges", AcceptRanges),
    ("age", Age),
    ("etag", ETag),
    ("location", Location),
    ("proxy-authenticate", ProxyAuthenticate),
    ("retry-after", RetryAfter),
    ("server", Server),
    ("vary", Vary),
    ("www-authenticate", WWWAuthenticate))
}
