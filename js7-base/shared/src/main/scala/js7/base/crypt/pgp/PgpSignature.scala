package js7.base.crypt.pgp

import js7.base.crypt.pgp.PgpSignature.*
import js7.base.crypt.{GenericSignature, Signature}
import js7.base.utils.ScalaUtils.syntax.*

/**
  * @author Joacim Zschimmer
  */
final case class PgpSignature(string: String) extends Signature:
  def toGenericSignature = GenericSignature(TypeName, string)

  override def toString = s"PgpSignature(${string.truncateWithEllipsis(20)})"

object PgpSignature:
  val TypeName = "PGP"
