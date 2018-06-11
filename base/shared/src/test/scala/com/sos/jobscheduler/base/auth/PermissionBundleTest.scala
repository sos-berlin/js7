package com.sos.jobscheduler.base.auth

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class PermissionBundleTest extends FreeSpec
{
  "isIncludedIn" in {
    case object A extends Permission
    case object B extends Permission

    assert(PermissionBundle.empty contains PermissionBundle(Set.empty))
    assert(PermissionBundle(Set(A)) contains PermissionBundle(Set.empty))
    assert(PermissionBundle(Set(A)) contains PermissionBundle(Set(A)))
    assert(PermissionBundle(Set(A, B)) contains PermissionBundle(Set(A)))

    assert(!PermissionBundle.empty.contains(PermissionBundle(Set(A))))
    assert(!PermissionBundle(Set(B)).contains(PermissionBundle(Set(A))))
    assert(!PermissionBundle(Set(B)).contains(PermissionBundle(Set(A, B))))
  }
}
