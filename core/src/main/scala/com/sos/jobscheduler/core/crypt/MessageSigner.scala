package com.sos.jobscheduler.core.crypt

import com.sos.jobscheduler.data.crypt.Signature

trait MessageSigner[S <: Signature]
{
  def sign(message: String): S
}
