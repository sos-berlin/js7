package com.sos.jobscheduler.common.auth

import com.sos.jobscheduler.base.generic.SecretString

/**
  * @author Joacim Zschimmer
  */
final case class RawUserAccount(encodedPassword: SecretString, permissions: Set[String])
