package js7.base.crypt

import js7.base.utils.ScalaUtils.syntax.RichString

/** `serialized` must contains the serialized form of `value`.
  *
  * @author Joacim Zschimmer
  */
final case class Signed[+A](value: A, signedString: SignedString)
{
  override def toString =
    s"Signed(${value.toString.truncateWithEllipsis(100, firstLineOnly = true)}, $signedString)"
}
