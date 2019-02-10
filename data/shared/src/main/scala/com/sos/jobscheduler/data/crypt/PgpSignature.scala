package com.sos.jobscheduler.data.crypt

import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.data.crypt.PgpSignature._

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
