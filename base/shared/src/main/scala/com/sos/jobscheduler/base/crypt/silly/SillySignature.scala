package com.sos.jobscheduler.base.crypt.silly

import com.sos.jobscheduler.base.crypt.silly.SillySignature._
import com.sos.jobscheduler.base.crypt.{GenericSignature, Signature}

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
