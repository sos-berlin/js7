package js7.base.crypt.donotcrypt

import js7.base.crypt.{GenericSignature, Signature}

/**
  * @author Joacim Zschimmer
  */
case object DoNotVerifySignature extends Signature:

  val TypeName = "DontCheck"

  override def toGenericSignature = GenericSignature(TypeName, "")
