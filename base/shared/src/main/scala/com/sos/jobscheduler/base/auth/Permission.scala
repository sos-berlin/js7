package com.sos.jobscheduler.base.auth

import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import scala.collection.immutable.Iterable

/**
  * @author Joacim Zschimmer
  */
trait Permission {
  def name: String = getClass.simpleScalaName stripSuffix "Permission"
}

object Permission
{
  def toStringToPermission(permissions: Iterable[Permission]) =
    permissions.toKeyedMap(_.name)
}

/** SuperPermission covering all permissions. */
case object SuperPermission extends Permission

/**
  * Anonymous does not have this permission, while all other users have this permission.
  * <p>
  * If required, access is permitted if
  * <ul>
  * <li> the user has this permission (user is not Anonymous)
  * <li> or the user is Anonymous and
  *   <ul>
  *   <li> loopbackIsPublic is set and access via a TCP port bound to a loopback interface (like localhost)
  *   <li> getIsPublic is set and access is via GET
  *   </ul>
  * </ul>
  * For implementation, see GateKeeper.isAllowed
  */
case object ValidUserPermission extends Permission

case object UpdateRepoPermission extends Permission
