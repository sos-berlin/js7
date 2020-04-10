package com.sos.jobscheduler.base.auth

import com.sos.jobscheduler.base.auth.Permission._
import com.sos.jobscheduler.base.auth.PermissionTest._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class PermissionTest extends AnyFreeSpec
{
  "toStringToPermission" in {
    assert(toStringToPermission(Set(TestPermission, AnotherPermission)) ==
      Map(
        "Test" -> TestPermission,
        "Another" -> AnotherPermission))
  }
}

private object PermissionTest
{
  private object TestPermission extends Permission
  private object AnotherPermission extends Permission
}
