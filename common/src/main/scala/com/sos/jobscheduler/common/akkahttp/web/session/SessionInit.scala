package com.sos.jobscheduler.common.akkahttp.web.session

import com.sos.jobscheduler.base.auth.{SessionToken, User}

/**
  * @author Joacim Zschimmer
  */
final case class SessionInit[U <: User](sessionToken: SessionToken, sessionNumber: Long, originalUser: U)
