package com.sos.jobscheduler.data.workflow.position

import com.sos.jobscheduler.data.workflow.position.BranchId.{catch_, try_}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class BranchIdTest extends FreeSpec
{
  "try_" in {
    intercept[IllegalArgumentException](try_(-1))
    assert(try_(0) == BranchId.Named("try"))
    assert(try_(1) == BranchId.Named("try+1"))
  }

  "catch_" in {
    intercept[IllegalArgumentException](catch_(-1))
    assert(catch_(0) == BranchId.Named("catch"))
    assert(catch_(1) == BranchId.Named("catch+1"))
  }
}
