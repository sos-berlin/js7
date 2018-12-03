package com.sos.jobscheduler.data.order

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderIdTest extends FreeSpec {

  "officialSyntaxChecked" in {
    assert(OrderId.checked("") == Invalid(Problem("OrderId must not be empty")))
    assert(OrderId("a/b").checkedNameSyntax == Invalid(Problem("OrderId must not contain reserved characters /")))

    assert(OrderId("a@b").checkedNameSyntax == Valid(OrderId("a@b")))
    assert(OrderId("1").checkedNameSyntax == Valid(OrderId("1")))
    assert(OrderId("A").checkedNameSyntax == Valid(OrderId("A")))
    assert(OrderId("A-_.B").checkedNameSyntax == Valid(OrderId("A-_.B")))
  }
}
