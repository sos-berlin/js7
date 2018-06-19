package com.sos.jobscheduler.common.akkahttp.web.session

import com.sos.jobscheduler.base.auth.SimpleUser

/**
  * @author Joacim Zschimmer
  */
final case class SimpleSession(sessionInit: SessionInit[SimpleUser]) extends Session {
  type User = SimpleUser
}
