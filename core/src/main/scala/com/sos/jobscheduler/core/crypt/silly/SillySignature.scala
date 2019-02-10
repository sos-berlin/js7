package com.sos.jobscheduler.core.crypt.silly

import com.sos.jobscheduler.core.crypt.silly.SillySignature._
import com.sos.jobscheduler.data.crypt.{GenericSignature, Signature}

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
