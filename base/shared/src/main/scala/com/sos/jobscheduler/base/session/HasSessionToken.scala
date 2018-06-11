package com.sos.jobscheduler.base.session

import com.sos.jobscheduler.base.auth.SessionToken

/**
  * @author Joacim Zschimmer
  */
trait HasSessionToken {
  protected def sessionToken: Option[SessionToken]
}
