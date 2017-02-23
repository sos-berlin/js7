package com.sos.scheduler.engine.base.convert

import java.nio.file.{Path, Paths}

/**
  * @author Joacim Zschimmer
  */
trait As[V, W] extends (V ⇒ W)

object As {

  def apply[V, W](asW: V ⇒ W): As[V, W] =
    new As[V, W] {
      def apply(v: V) = asW(v)
    }

  def convert[V, W](from: V)(implicit to: As[V, W]): W =
    to(from)

  implicit val StringAsString: As[String, String] = As(identity)

  implicit object StringAsBoolean extends As[String, Boolean] {
    val StringToBooleanMap = Map(
      "true"  → true , "on"  → true , "yes" → true,
      "false" → false, "off" → false, "no"  → false)

    def apply(o: String) = StringToBooleanMap.getOrElse(o,
      throw new IllegalArgumentException(s"Boolean value true or false expected, not: $o"))
  }

  //
  // Some default implementations
  //

  implicit val StringAsInt: As[String, Int] =
    As(Integer.parseInt)

  implicit val StringAsLong: As[String, Long] =
    As(java.lang.Long.parseLong)

  def asAbsolutePath: As[String, Path] =
    As(o ⇒ StringAsPath(o).toAbsolutePath)

  implicit val StringAsPath: As[String, Path] =
    As(Paths.get(_))
}
