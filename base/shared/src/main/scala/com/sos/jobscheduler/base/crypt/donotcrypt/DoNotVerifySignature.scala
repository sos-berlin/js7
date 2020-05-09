package com.sos.jobscheduler.base.crypt.donotcrypt

import com.sos.jobscheduler.base.crypt.{GenericSignature, Signature}

/**
  * @author Joacim Zschimmer
  */
case object DoNotVerifySignature extends Signature
{
  val TypeName = "DontCheck"

  override def toGenericSignature = GenericSignature(TypeName, "")
}
