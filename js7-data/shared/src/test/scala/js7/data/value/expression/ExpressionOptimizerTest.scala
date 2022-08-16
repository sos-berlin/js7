package js7.data.value.expression

import js7.base.test.OurTestSuite
import js7.data.value.expression.Expression.{ListExpression, MkString, NumericConstant, StringConstant}
import js7.data.value.expression.ExpressionOptimizer.optimizeExpression

final class ExpressionOptimizerTest extends OurTestSuite
{
  "[].mkString" in {
    assert(optimizeExpression(MkString(ListExpression(Nil))) == StringConstant.empty)
  }

  """["X"].mkString""" in {
    assert(optimizeExpression(MkString(ListExpression(List(StringConstant("X"))))) ==
      StringConstant("X"))
  }

  """[1].mkString""" in {
    assert(optimizeExpression(MkString(ListExpression(List(NumericConstant(1))))) ==
      MkString(NumericConstant(1)))
  }

  """["", 1, "", "A", ""].mkString""" in {
    assert(optimizeExpression(
      MkString(ListExpression(List(
        StringConstant.empty, NumericConstant(1), StringConstant.empty, StringConstant("A"), StringConstant.empty))))
      == MkString(ListExpression(List(NumericConstant(1), StringConstant("A")))))
  }

  """["A", "B", "C"].mkString""" in {
    assert(optimizeExpression(MkString(ListExpression(List(StringConstant("A"), StringConstant("B"), StringConstant("C")))))
      == StringConstant("ABC"))
  }

  """["A", "B", "C", 1, "C", "D"].mkString""" in {
    assert(
      optimizeExpression(MkString(ListExpression(List(
        StringConstant("A"), StringConstant("B"), StringConstant("C"),
        NumericConstant(1),
        StringConstant("C"), StringConstant("D")))))
      == MkString(ListExpression(List(StringConstant("ABC"), NumericConstant(1), StringConstant("CD")))))
  }

  """["A", [ "B", [ "C", "D" ]]].mkString""" in {
    assert(
      optimizeExpression(
        MkString(ListExpression(List(
        StringConstant("A"),
        MkString(ListExpression(List(
          StringConstant("B"),
          MkString(ListExpression(List(
            StringConstant("C"),
            StringConstant("D")))))))))))
      == StringConstant("ABCD"))
  }
}
