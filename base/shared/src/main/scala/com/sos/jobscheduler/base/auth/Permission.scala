package com.sos.jobscheduler.base.auth

/**
  * @author Joacim Zschimmer
  */
trait Permission

/**
  * Anonymous does not have this permission, while all other users have this permission.
  * <p>
  * If required, access is permitted if
  * <ul>
  * <li> the user has this permission (user is not Anonymous)
  * <li> or the user is Anonymous and
  *   <ul>
  *   <li> loopbackAllowsAnonymous is set and access via a TCP port bound to a loopback interface (like localhost)
  *   <li> getAllowsAnonymous is set and access is via GET
  *   </ul>
  * </ul>
  * For implementation, see GateKeeper.isAllowed
  */
case object ValidUserPermission extends Permission
