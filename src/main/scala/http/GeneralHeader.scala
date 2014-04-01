package http

sealed trait GeneralHeader {
  val asString: String
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9">§</a>
  */
final case object CacheControl extends GeneralHeader {
  /**
    * A string representation of this general header.
    */
  override val asString = "Cache-Control"
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.10">§</a>
  */
final case object Connection extends GeneralHeader {
  /**
    * A string representation of this general header.
    */
  override val asString = toString
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.18">§</a>
  */
final case object Date extends GeneralHeader {
  /**
    * A string representation of this general header.
    */
  override val asString = toString
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.32">§</a>
  */
final case object Pragma extends GeneralHeader {
  /**
    * A string representation of this general header.
    */
  override val asString = toString
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.40">§</a>
  */
final case object Trailer extends GeneralHeader {
  /**
    * A string representation of this general header.
    */
  override val asString = toString
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.41">§</a>
  */
final case object TransferEncoding extends GeneralHeader {
  /**
    * A string representation of this general header.
    */
  override val asString = "Transfer-Encoding"
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.42">§</a>
  */
final case object Upgrade extends GeneralHeader {
  /**
    * A string representation of this general header.
    */
  override val asString = toString
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.45">§</a>
  */
final case object Via extends GeneralHeader {
  /**
    * A string representation of this general header.
    */
  override val asString = toString
}
/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.46">§</a>
  */
final case object Warning extends GeneralHeader {
  /**
    * A string representation of this general header.
    */
  override val asString = toString
}

object GeneralHeader {
  def from(s: String) = s.toLowerCase match {
    case "cache-control" => Some(CacheControl)
    case "connection" => Some(Connection)
    case "date" => Some(Date)
    case "pragma" => Some(Pragma)
    case "trailer" => Some(Trailer)
    case "transfer-encoding" => Some(TransferEncoding)
    case "upgrade" => Some(Upgrade)
    case "via" => Some(Via)
    case "warning" => Some(Warning)
    case _ => None
  }
}
