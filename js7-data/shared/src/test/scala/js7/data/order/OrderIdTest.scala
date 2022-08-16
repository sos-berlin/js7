package js7.data.order

import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Problem
import js7.base.test.Test

/**
  * @author Joacim Zschimmer
  */
final class OrderIdTest extends Test {

  "checkedNameSyntax" in {
    assert(OrderId.checked("") == Left(EmptyStringProblem("OrderId")))
    assert(OrderId("a|b").checkedNameSyntax == Left(Problem("OrderId must not contain reserved characters: |")))

    assert(OrderId("a/b").checkedNameSyntax == Right(OrderId("a/b")))
    assert(OrderId("a@b").checkedNameSyntax == Right(OrderId("a@b")))
    assert(OrderId("1").checkedNameSyntax == Right(OrderId("1")))
    assert(OrderId("A").checkedNameSyntax == Right(OrderId("A")))
    assert(OrderId("A-_.B").checkedNameSyntax == Right(OrderId("A-_.B")))
  }

  "allParents" in {
    assert(OrderId("A").allParents == Nil)
    assert(OrderId("A|B").allParents == Seq(OrderId("A")))
    assert(OrderId("A|B|C").allParents == Seq(OrderId("A"), OrderId("A|B")))
    assert(OrderId("A|BBB|C|D").allParents == Seq(OrderId("A"), OrderId("A|BBB"), OrderId("A|BBB|C")))
  }

  "root" in {
    assert(OrderId("A").root == OrderId("A"))
    assert(OrderId("A|B").root == OrderId("A"))
    assert(OrderId("A|B|C").root == OrderId("A"))
  }

  "isRoot" in {
    assert(OrderId("A").isRoot)
    assert(!OrderId("A|B").isRoot)
  }
}
