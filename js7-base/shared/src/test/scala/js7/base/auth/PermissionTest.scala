package js7.base.auth

import js7.base.auth.Permission.*
import js7.base.auth.PermissionTest.*
import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class PermissionTest extends OurTestSuite:
  "toStringToPermission" in:
    assert(toStringToPermission(Set(TestPermission, AnotherPermission)) ==
      Map(
        "Test" -> TestPermission,
        "Another" -> AnotherPermission))

private object PermissionTest:
  private object TestPermission extends Permission
  private object AnotherPermission extends Permission
