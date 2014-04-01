package http

import scalaz._

sealed trait Version {
  val major: Digit

  val minor: Digit

  override def toString = "HTTP/" + major.toInt + "." + minor.toInt
}

object Version {
  def version(maj: Digit, min: Digit) = new Version {
    val major = maj
    val minor = min
  }

  /** HTTP/1.0 */
  val version10 = version(Digit._1, Digit._0)

  /** HTTP/1.1 */
  val version11 = version(Digit._1, Digit._1)

  implicit object VersionShow extends Show[Version] {
    override def shows(a: Version) =
      "HTTP/" + a.major.toInt + "." + a.minor.toInt
  }
}
