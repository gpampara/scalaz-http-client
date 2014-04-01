package http
package request

sealed trait RequestHeader {
  val asString: String
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1">§</a>
  */
final case object Accept extends RequestHeader {
  override val asString = toString
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.2">§</a>
  */
final case object AcceptCharset extends RequestHeader {
  override val asString = "Accept-Charset"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.3">§</a>
  */
final case object AcceptEncoding extends RequestHeader {
  override val asString = "Accept-Encoding"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.4">§</a>
  */
final case object AcceptLanguage extends RequestHeader {
  override val asString = "Accept-Language"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.8">§</a>
  */
final case object Authorization extends RequestHeader {
  override val asString = toString
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.20">§</a>
  */
final case object Expect extends RequestHeader {
  override val asString = toString
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.22">§</a>
  */
final case object From extends RequestHeader {
  override val asString = toString
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.23">§</a>
  */
final case object Host extends RequestHeader {
  override val asString = toString
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.24">§</a>
  */
final case object IfMatch extends RequestHeader {
  override val asString = "If-Match"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.25">§</a>
  */
final case object IfModifiedSince extends RequestHeader {
  override val asString = "If-Modified-Since"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.26">§</a>
  */
final case object IfNoneMatch extends RequestHeader {
  override val asString = "If-None-Match"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.27">§</a>
  */
final case object IfRange extends RequestHeader {
  override val asString = "If-Range"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.28">§</a>
  */
final case object IfUnmodifiedSince extends RequestHeader {
  override val asString = "If-Unmodified-Since"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.31">§</a>
  */
final case object MaxForwards extends RequestHeader {
  override val asString = "Max-Forwards"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.34">§</a>
  */
final case object ProxyAuthorization extends RequestHeader {
  override val asString = "Proxy-Authorization"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35">§</a>
  */
final case object Range extends RequestHeader {
  override val asString = toString
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.36">§</a>
  */
final case object Referer extends RequestHeader {
  override val asString = toString
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.39">§</a>
  */
final case object TE extends RequestHeader {
  override val asString = toString
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.43">§</a>
  */
final case object UserAgent extends RequestHeader {
  override val asString = "User-Agent"
}

object RequestHeader {
  implicit class Entity(eh: EntityHeader) extends RequestHeader {
    override val asString = eh.asString
  }

  implicit class General(gh: GeneralHeader) extends RequestHeader {
    override val asString = gh.asString
  }

  def from(s: String): Option[RequestHeader] =
    headers find { case (n, h) => n.equalsIgnoreCase(s) } map (_._2) orElse
  GeneralHeader.from(s).map(General(_)) orElse
  EntityHeader.from(s).map(Entity(_))

  val headers = List(("accept", Accept),
    ("accept-charset", AcceptCharset),
    ("accept-encoding", AcceptEncoding),
    ("accept-language", AcceptLanguage),
    ("authorization", Authorization),
    ("from", From),
    ("host", Host),
    ("if-match", IfMatch),
    ("if-modified-since", IfModifiedSince),
    ("if-none-match", IfNoneMatch),
    ("if-range", IfRange),
    ("if-unmodified-since", IfUnmodifiedSince),
    ("max-forwards", MaxForwards),
    ("proxy-authorization", ProxyAuthorization),
    ("range", Range),
    ("referer", Referer),
    ("te", TE),
    ("user-agent", UserAgent))
}
