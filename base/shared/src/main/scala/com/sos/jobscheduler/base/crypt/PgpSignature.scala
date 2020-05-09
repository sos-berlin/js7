package com.sos.jobscheduler.base.crypt

import com.sos.jobscheduler.base.crypt.PgpSignature._
import com.sos.jobscheduler.base.utils.Strings.RichString

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
