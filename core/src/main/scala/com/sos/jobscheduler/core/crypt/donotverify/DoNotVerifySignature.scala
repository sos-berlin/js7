package com.sos.jobscheduler.core.crypt.donotverify

import com.sos.jobscheduler.data.crypt.{GenericSignature, Signature}

/**
  * @author Joacim Zschimmer
  */
case object DoNotVerifySignature extends Signature
{
  val TypeName = "DontCheck"

  override def toGenericSignature = GenericSignature(TypeName, "")
}
