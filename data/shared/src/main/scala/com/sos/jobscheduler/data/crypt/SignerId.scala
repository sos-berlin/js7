package com.sos.jobscheduler.data.crypt

import com.sos.jobscheduler.base.generic.GenericString

/**
  * Name, comment and email.
  * PGP does not seem to require uniqueness.
  * @author Joacim Zschimmer
  */
final case class SignerId(string: String) extends GenericString
