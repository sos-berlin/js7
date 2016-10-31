package com.sos.scheduler.engine.common.auth

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
  object Anonymous extends User {
    def id = UserId.Anonymous
  }
}
