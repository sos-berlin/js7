package js7.data.value.expression

import js7.base.problem.{Checked, Problem}
import js7.base.test.Test
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.Scope.evalLazilyExpressions
import js7.data.value.{NumberValue, StringValue, Value}
import scala.collection.MapView

final class ScopeTest extends Test
{
  "evalLazilyExpressions" - {
    var a = 0
    var failed = 0
    implicit lazy val nameToValue: MapView[String, Checked[Value]] =
      evalLazilyExpressions(MapView(
        "A" -> new Expression.ImpureTest(() => {
          a += 1
          Right(NumberValue(a))
        }),
        "B" -> StringConstant("BBB"),
        "FAIL" -> new Expression.ImpureTest(() => {
          failed += 1
          Left(Problem("FAILED"))
        })
      ))(Scope.empty)

    "Each entry is lazily evaluated only once" in {
      implicit val scope = Scope.empty
      assert(a == 0)
      assert(nameToValue.get("A") == Some(Right(NumberValue(1))))
      assert(a == 1)
      assert(nameToValue.get("A") == Some(Right(NumberValue(1))))
      assert(a == 1)
      assert(nameToValue.get("B") == Some(Right(StringValue("BBB"))))
    }

    "Even if evaluation fails" in {
      assert(failed == 0)
      assert(nameToValue.get("FAIL") == Some(Left(Problem("FAILED"))))
      assert(failed == 1)
      assert(nameToValue.get("FAIL") == Some(Left(Problem("FAILED"))))
      assert(failed == 1)
      assert(nameToValue.get("A") == Some(Right(NumberValue(1))))
      assert(nameToValue.get("B") == Some(Right(StringValue("BBB"))))
    }
  }
}
