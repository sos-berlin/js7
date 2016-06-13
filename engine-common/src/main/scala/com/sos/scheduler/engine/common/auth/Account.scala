package com.sos.scheduler.engine.common.auth

/**
  * Account for a user.
  *
  * @author Joacim Zschimmer
  */
trait Account {
  /** The user ID **/
  def id: String
}
