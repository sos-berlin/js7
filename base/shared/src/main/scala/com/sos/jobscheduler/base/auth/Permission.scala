package com.sos.jobscheduler.base.auth

/**
  * @author Joacim Zschimmer
  */
trait Permission

/**
  * Anonymous does not have this permission, while all other users have this permission.
  *
  * If required, access is permitted if
  * - the user has this permission (user is not Anonymous)
  * - or the user is Anonymous and
  *   - httpIsPublic is set and access is via HTTP but not HTTPS
  *   - getIsPublic is set and access is via GET
  */
case object KnownUserPermission extends Permission
