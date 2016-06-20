package com.sos.scheduler.engine.common.convert

import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.time.ScalaTime.parseDuration
import java.nio.file.{Path, Paths}
import java.time.Duration

/**
  * @author Joacim Zschimmer
  */
trait As[V, W] extends (V ⇒ W)

object As {
  def apply[V, W](asW: V ⇒ W): As[V, W] = new As[V, W] { def apply(v: V) = asW(v) }

  implicit val StringAsString: As[String, String] = stringAs(identity)

  implicit object StringAsBoolean extends As[String, Boolean] {
    val StringToBooleanMap = Map(
      "true" → true, "on" → true, "yes" → true,
      "false" → false, "off" → false, "no" → false)

    def apply(o: String) = StringToBooleanMap.getOrElse(o,
      throw new IllegalArgumentException(s"Boolean value true or false expected, not: $o"))
  }

  implicit val StringAsInt: As[String, Int] = stringAs(Integer.parseInt)

  def asAbsolutePath: As[String, Path] = stringAs(o ⇒ StringAsPath(o).toAbsolutePath)

  implicit val StringAsPath: As[String, Path] = stringAs(Paths.get(_))

  implicit val StringAsSecretString: As[String, SecretString] = stringAs(SecretString.apply)

  implicit val StringAsDuration: As[String, Duration] = stringAs(parseDuration)

  def stringAs[A](convert: String ⇒ A) = As[String, A](convert)
}
