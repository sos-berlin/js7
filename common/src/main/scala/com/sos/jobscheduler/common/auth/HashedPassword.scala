package com.sos.jobscheduler.common.auth

import com.sos.jobscheduler.base.generic.SecretString

/**
  * @author Joacim Zschimmer
  */
final case class HashedPassword(hashed: SecretString, hasher: String ⇒ String)
