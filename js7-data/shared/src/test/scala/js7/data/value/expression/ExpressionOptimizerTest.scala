package js7.data.value.expression

import js7.base.test.OurTestSuite
import js7.data.value.expression.Expression.{ListExpr, MkString, NumericConstant, StringConstant}
import js7.data.value.expression.ExpressionOptimizer.optimize

final class ExpressionOptimizerTest extends OurTestSuite:
  "[].mkString" in:
    assert(optimize(MkString(ListExpr(Nil))) == StringConstant.empty)

  """["X"].mkString""" in:
    assert(optimize(MkString(ListExpr(List(StringConstant("X"))))) ==
      StringConstant("X"))

  """[1].mkString""" in:
    assert(optimize(MkString(ListExpr(List(NumericConstant(1))))) ==
      MkString(NumericConstant(1)))

  """["", 1, "", "A", ""].mkString""" in:
    assert(optimize(
      MkString(ListExpr(List(
        StringConstant.empty, NumericConstant(1), StringConstant.empty, StringConstant("A"), StringConstant.empty))))
      == MkString(ListExpr(List(NumericConstant(1), StringConstant("A")))))

  """["A", "B", "C"].mkString""" in:
    assert(optimize(MkString(ListExpr(List(StringConstant("A"), StringConstant("B"), StringConstant("C")))))
      == StringConstant("ABC"))

  """["A", "B", "C", 1, "C", "D"].mkString""" in:
    assert(
      optimize(MkString(ListExpr(List(
        StringConstant("A"), StringConstant("B"), StringConstant("C"),
        NumericConstant(1),
        StringConstant("C"), StringConstant("D")))))
      == MkString(ListExpr(List(StringConstant("ABC"), NumericConstant(1), StringConstant("CD")))))

  """["A", [ "B", [ "C", "D" ]]].mkString""" in:
    assert(
      optimize(
        MkString(ListExpr(List(
        StringConstant("A"),
        MkString(ListExpr(List(
          StringConstant("B"),
          MkString(ListExpr(List(
            StringConstant("C"),
            StringConstant("D")))))))))))
      == StringConstant("ABCD"))
