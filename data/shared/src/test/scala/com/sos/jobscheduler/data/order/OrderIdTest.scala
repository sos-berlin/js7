package com.sos.jobscheduler.data.order

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderIdTest extends FreeSpec {

  "officialSyntaxChecked" in {
    assert(OrderId("A").checkedNameSyntax == Valid(OrderId("A")))
    assert(OrderId("A-_.B").checkedNameSyntax == Valid(OrderId("A-_.B")))
    assert(OrderId("a@b").checkedNameSyntax == Invalid(Problem("Problem with 'OrderId': Invalid character or character combination in name 'a@b'")))
    assert(OrderId("a/b").checkedNameSyntax == Invalid(Problem("Problem with 'OrderId': Invalid character or character combination in name 'a/b'")))
  }
}
