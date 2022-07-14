package js7.base.crypt.silly

import js7.base.crypt.silly.SillySignature.*
import js7.base.crypt.{GenericSignature, Signature}

/**
  * @author Joacim Zschimmer
  */
final case class SillySignature(string: String) extends Signature
{
  def toGenericSignature = GenericSignature(TypeName, string)
}

object SillySignature
{
  def Default = SillySignature("SILLY-SIGNATURE")

  val TypeName = "Silly"
}
