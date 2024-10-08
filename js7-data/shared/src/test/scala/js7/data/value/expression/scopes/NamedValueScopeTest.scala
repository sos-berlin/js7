package js7.data.value.expression.scopes

import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.data.value.expression.Expression.NamedValue
import js7.data.value.expression.Scope
import js7.data.value.{NumberValue, StringValue}

final class NamedValueScopeTest extends OurTestSuite:

  private var a = 0
  private implicit lazy val scope: Scope =
    NamedValueScope:
      case "A" =>
        a += 1
        Right(NumberValue(100))
      case "B" => Right(StringValue("BBB"))
      case "FAIL" => Left(Problem("FAILED"))

  "eval" in:
    assert(NamedValue("B").eval == Right(StringValue("BBB")))
    assert(a == 0)
    assert(NamedValue("A").eval == Right(NumberValue(100)))
    assert(a == 1)
    assert(NamedValue("A").eval == Right(NumberValue(100)))
    assert(a == 2)
    assert(NamedValue("FAIL").eval == Left(Problem("FAILED")))

  "namedValue" in:
    assert(scope.nameToCheckedValue.lift("A") == Some(Right(NumberValue(100))))
    assert(scope.nameToCheckedValue.lift("X") == None)
