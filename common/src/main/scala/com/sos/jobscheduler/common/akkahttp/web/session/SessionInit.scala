package com.sos.jobscheduler.common.akkahttp.web.session

import com.sos.jobscheduler.base.auth.{SessionToken, User}

/**
  * @author Joacim Zschimmer
  */
final case class SessionInit[U <: User](
  sessionNumber: Long,
  sessionToken: SessionToken,
  /** If isAnonymous, the Session's user may change later due to late authentication. */
  loginUser: U)
