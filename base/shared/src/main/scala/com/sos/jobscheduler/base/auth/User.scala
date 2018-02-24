package com.sos.jobscheduler.base.auth

/**
  * User for a user.
  *
  * @author Joacim Zschimmer
  */
trait User {
  def id: UserId
}

object User {
  /** The unauthenticated, anonymous user. */
  case object Anonymous extends User {
    def id = UserId.Anonymous
  }
}
