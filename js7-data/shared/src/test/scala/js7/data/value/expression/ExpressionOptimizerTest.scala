package js7.data.value.expression

import js7.base.test.OurTestSuite
import js7.data.value.expression.Expression.convenience.given
import js7.data.value.expression.Expression.{ListExpr, MkString, NumericConstant, StringConstant}
import js7.data.value.expression.ExpressionOptimizer.optimize
import scala.language.implicitConversions

final class ExpressionOptimizerTest extends OurTestSuite:

  "[].mkString" in:
    assert(optimize(MkString(ListExpr.empty)) == StringConstant.empty)

  """["X"].mkString""" in:
    assert(optimize(MkString(ListExpr.of("X"))) == StringConstant("X"))

  """[1].mkString""" in:
    assert(optimize(MkString(ListExpr.of(1))) == MkString(NumericConstant(1)))

  """["", 1, "", "A", ""].mkString""" in:
    assert(optimize(
      MkString(ListExpr.of("", 1, "", "A", ""))) == MkString(ListExpr.of(1, "A")))

  """["A", "B", "C"].mkString""" in:
    assert(optimize(MkString(ListExpr.of("A", "B", "C"))) == StringConstant("ABC"))

  """["A", "B", "C", 1, "C", "D"].mkString""" in:
    assert:
      optimize(MkString(ListExpr.of("A", "B", "C", 1, "C", "D")))
        == MkString(ListExpr.of("ABC", 1, "CD"))

  """["A", [ "B", [ "C", "D" ]]].mkString""" in:
    assert:
      optimize:
        MkString(ListExpr.of(
          "A",
          MkString(ListExpr.of(
            "B",
            MkString(ListExpr.of(
              "C",
              "D"))))))
      == StringConstant("ABCD")
