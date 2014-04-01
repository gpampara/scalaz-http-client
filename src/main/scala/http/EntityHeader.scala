package http

import scalaz._

sealed trait EntityHeader {
  val asString: String

  lazy val isExtension = this match {
    case ExtensionHeader(_) => true
    case _ => false
  }
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.7">§</a>
  */
final case object Allow extends EntityHeader {
  /**
    * A string representation of this entity header.
    */
  override val asString = toString
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11">§</a>
  */
final case object ContentEncoding extends EntityHeader {
  /**
    * A string representation of this entity header.
    */
  override val asString = "Content-Encoding"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.12">§</a>
  */
final case object ContentLanguage extends EntityHeader {
  /**
    * A string representation of this entity header.
    */
  override val asString = "Content-Language"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13">§</a>
  */
final case object ContentLength extends EntityHeader {
  /**
    * A string representation of this entity header.
    */
  override val asString = "Content-Length"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.14">§</a>
  */
final case object ContentLocation extends EntityHeader {
  /**
    * A string representation of this entity header.
    */
  override val asString = "Content-Location"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.15">§</a>
  */
final case object ContentMD5 extends EntityHeader {
  /**
    * A string representation of this entity header.
    */
  override val asString = "Content-MD5"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.16">§</a>
  */
final case object ContentRange extends EntityHeader {
  /**
    * A string representation of this entity header.
    */
  override val asString = "Content-Range"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17">§</a>
  */
final case object ContentType extends EntityHeader {
  /**
    * A string representation of this entity header.
    */
  override val asString = "Content-Type"
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21">§</a>
  */
final case object Expires extends EntityHeader {
  /**
    * A string representation of this entity header.
    */
  override val asString = toString
}

/**
  * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.29">§</a>
  */
final case object LastModified extends EntityHeader {
  /**
    * A string representation of this entity header.
    */
  override val asString = "Last-Modified"
}

private final case class ExtensionHeader(name: NonEmptyList[Char]) extends EntityHeader {
  override val asString = name.list.mkString
}

object EntityHeader {
  def from(s: String): Option[EntityHeader] = if (s.length == 0) None else Some(s.toLowerCase match {
    case "allow" => Allow
    case "content-encoding" => ContentEncoding
    case "content-language" => ContentLanguage
    case "content-length" => ContentLength
    case "content-location" => ContentLocation
    case "content-md5" => ContentMD5
    case "content-range" => ContentRange
    case "content-type" => ContentType
    case "expires" => Expires
    case "last-modified" => LastModified
    case h => {
      val t: List[Char] = (s: scala.collection.immutable.StringOps).toList
      ExtensionHeader(NonEmptyList.nel(t.head, t.tail))
    }
  })
}
