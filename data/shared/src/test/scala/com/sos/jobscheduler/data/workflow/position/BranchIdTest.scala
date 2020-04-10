package com.sos.jobscheduler.data.workflow.position

import com.sos.jobscheduler.data.workflow.position.BranchId.{catch_, try_}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class BranchIdTest extends AnyFreeSpec
{
  "try_" in {
    intercept[IllegalArgumentException](try_(-1))
    assert(try_(0) == BranchId.Named("try+0"))
    assert(try_(1) == BranchId.Named("try+1"))
  }

  "catch_" in {
    intercept[IllegalArgumentException](catch_(-1))
    assert(catch_(0) == BranchId.Named("catch+0"))
    assert(catch_(1) == BranchId.Named("catch+1"))
  }
}
