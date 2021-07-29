package js7.data.value.expression

import fastparse.NoWhitespace._
import js7.base.problem.Checked
import js7.data.parser.Parsers.checkedParse
import js7.data.value.expression.Expression.{Add, Multiply, NamedValue, NumericConstant}
import js7.data.value.expression.ExpressionParser.functionOnly
import js7.data.value.expression.scopes.NamedValueScope
import js7.data.value.{ListValue, NamedValues, NumberValue, Value}
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec

final class ExprFunctionTest extends AnyFreeSpec
{
  testEval("() => 1", Scope.empty,
    args = ListValue(Nil),
    result = Right(NumberValue(1)),
    Right(ExprFunction(Nil, NumericConstant(1))))

  testEval("(a) => $a + 1", Scope.empty,
    args = ListValue(Seq(NumberValue(2))),
    result = Right(NumberValue(3)),
    Right(ExprFunction(Seq(VariableDeclaration("a")), Add(NamedValue("a"), NumericConstant(1)))))

  testEval("(a, b) => $a + 2 * $b", Scope.empty,
    args = ListValue(Seq(NumberValue(10), NumberValue(3))),
    result = Right(NumberValue(10 + 2 * 3)),
    Right(ExprFunction(
      Seq(
        VariableDeclaration("a"),
        VariableDeclaration("b")),
      Add(
        NamedValue("a"),
        Multiply(
          NumericConstant(2),
          NamedValue("b"))))))

  testEval("() => $nameFromContext",
    scope = NamedValueScope(NamedValues("nameFromContext" -> NumberValue(7))),
    args = ListValue(Nil),
    result = Right(NumberValue(7)),
    Right(ExprFunction(Nil, NamedValue("nameFromContext"))))

  private def testEval(
    exprString: String,
    scope: Scope,
    args: ListValue,
    result: Checked[Value],
    checkedFunction: Checked[ExprFunction])
    (implicit pos: source.Position): Unit =
    registerTest(exprString) {
      val checked = checkedParse(exprString.trim, functionOnly(_))
      assert(checked == checkedFunction)
      for (function <- checkedFunction) {
        assert(checkedParse(function.toString, functionOnly(_)) == checkedFunction, " in toString❗")
        assert(function.eval(args)(scope) == result)
      }
    }
}
