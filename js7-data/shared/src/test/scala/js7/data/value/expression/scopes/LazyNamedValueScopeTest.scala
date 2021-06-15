package js7.data.value.expression.scopes

import js7.base.problem.Problem
import js7.base.utils.Lazy
import js7.data.value.expression.Expression.NamedValue
import js7.data.value.{NumberValue, StringValue}
import org.scalatest.freespec.AnyFreeSpec

final class LazyNamedValueScopeTest extends AnyFreeSpec
{
  private var a = 0
  private var failed = 0
  private lazy val scope = new LazyNamedValueScope(Map(
    "A" -> Lazy {
      a += 1
      Right(NumberValue(a))
    },
    "B" -> Lazy(Right(StringValue("BBB"))),
    "FAIL" -> Lazy {
      failed += 1
      Left(Problem("FAILED"))
    }))

  "eval" in {
    assert(scope.evaluator.eval(NamedValue("B")) == Right(StringValue("BBB")))
    assert(a == 0)
    assert(scope.evaluator.eval(NamedValue("A")) == Right(NumberValue(1)))
    assert(a == 1)
    assert(scope.evaluator.eval(NamedValue("A")) == Right(NumberValue(1)))
    assert(a == 1)
  }

  "namedValue" in {
    assert(scope.namedValue("A") == Right(Some(NumberValue(1))))
    assert(scope.namedValue("X") == Right(None))
  }

  "Failure" in {
    assert(failed == 0)
    assert(scope.namedValue("FAIL") == Left(Problem("FAILED")))
    assert(failed == 1)
    assert(scope.namedValue("FAIL") == Left(Problem("FAILED")))
    assert(failed == 1)
    assert(scope.namedValue("A") == Right(Some(NumberValue(1))))
  }
}
