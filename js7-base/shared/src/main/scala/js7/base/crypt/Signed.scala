package js7.base.crypt

/** `serialized` must contains the serialized form of `value`.
  *
  * @author Joacim Zschimmer
  */
final case class Signed[+A](value: A, signedString: SignedString)
