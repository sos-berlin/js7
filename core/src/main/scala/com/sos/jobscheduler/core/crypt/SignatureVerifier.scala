package com.sos.jobscheduler.core.crypt

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.crypt.{Signature, SignerId}
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait SignatureVerifier[S <: Signature]
{
  def verify(message: String, signature: S): Checked[Seq[SignerId]]
}
