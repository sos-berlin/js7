package js7.base.crypt

import js7.base.crypt.PgpSignature._
import js7.base.utils.Strings.RichString

/**
  * @author Joacim Zschimmer
  */
final case class PgpSignature(string: String) extends Signature
{
  def toGenericSignature = GenericSignature(TypeName, string)

  override def toString = s"PgpSignature(${string.truncateWithEllipsis(20)})"
}

object PgpSignature
{
  val TypeName = "PGP"
}
