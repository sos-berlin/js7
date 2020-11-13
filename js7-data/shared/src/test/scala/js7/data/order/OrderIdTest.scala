package js7.data.order

import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Problem
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderIdTest extends AnyFreeSpec {

  "officialSyntaxChecked" in {
    assert(OrderId.checked("") == Left(EmptyStringProblem("OrderId")))
    assert(OrderId("a|b").checkedNameSyntax == Left(Problem("OrderId must not contain reserved characters |")))

    assert(OrderId("a/b").checkedNameSyntax == Right(OrderId("a/b")))
    assert(OrderId("a@b").checkedNameSyntax == Right(OrderId("a@b")))
    assert(OrderId("1").checkedNameSyntax == Right(OrderId("1")))
    assert(OrderId("A").checkedNameSyntax == Right(OrderId("A")))
    assert(OrderId("A-_.B").checkedNameSyntax == Right(OrderId("A-_.B")))
  }
}
