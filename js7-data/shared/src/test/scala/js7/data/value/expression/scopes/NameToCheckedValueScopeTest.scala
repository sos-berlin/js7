package js7.data.value.expression.scopes

import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.data.value.expression.Expression.NamedValue
import js7.data.value.expression.{Scope, ValueSearch}
import js7.data.value.{NumberValue, StringValue}
import scala.collection.MapView

final class NameToCheckedValueScopeTest extends OurTestSuite:
  private var a = 0
  private implicit lazy val scope: Scope =
    NameToCheckedValueScope(MapView(
      "A" -> {
        a += 1
        Right(NumberValue(a))
      },
      "B" -> Right(StringValue("BBB")),
      "FAIL" -> Left(Problem("FAILED"))))

  "eval" in:
    assert(NamedValue("B").eval == Right(StringValue("BBB")))
    assert(a == 1)
    assert(NamedValue("A").eval == Right(NumberValue(1)))
    assert(a == 1)
    assert(NamedValue("A").eval == Right(NumberValue(1)))
    assert(NamedValue("FAIL").eval == Left(Problem("FAILED")))

  "findValue" in:
    import ValueSearch.{Argument, LastOccurred, Name}

    assert(scope.findValue(ValueSearch(LastOccurred, Name("A"))) == Some(Right(NumberValue(1))))
    assert(scope.findValue(ValueSearch(LastOccurred, Name("X"))) == None)
    assert(scope.findValue(ValueSearch(Argument, Name("A"))) == None)
