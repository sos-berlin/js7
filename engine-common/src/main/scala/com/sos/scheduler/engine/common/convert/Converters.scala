package com.sos.scheduler.engine.common.convert

import com.sos.scheduler.engine.base.generic.SecretString
import java.nio.file.{Path, Paths}

/**
  * @author Joacim Zschimmer
  */
object Converters {

  trait To[V, W] extends (V ⇒ W)

  object To {
    def apply[V, W](toW: V ⇒ W): To[V, W] = new To[V, W] { def apply(v: V) = toW(v) }
  }

  implicit val StringToString: To[String, String] = stringTo(identity)

  implicit object StringToBoolean extends To[String, Boolean] {
    val StringToBooleanMap = Map(
      "true" → true, "on" → true, "yes" → true,
      "false" → false, "off" → false, "no" → false)

    def apply(o: String) = StringToBooleanMap.getOrElse(o,
      throw new IllegalArgumentException(s"Boolean value true or false expected, not: $o"))
  }

  implicit val StringToInt: To[String, Int] = stringTo(Integer.parseInt)

  def toAbsolutePath: To[String, Path] = stringTo(o ⇒ StringToPath(o).toAbsolutePath)

  implicit val StringToPath: To[String, Path] = stringTo(Paths.get(_))

  implicit val StringToSecretString: To[String, SecretString] = stringTo(SecretString.apply)

  def stringTo[A](convert: String ⇒ A) = To[String, A](convert)
}
