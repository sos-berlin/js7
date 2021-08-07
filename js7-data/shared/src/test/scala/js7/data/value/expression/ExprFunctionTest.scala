package js7.data.value.expression

import fastparse.NoWhitespace._
import js7.base.problem.{Checked, Problem}
import js7.data.parser.Parsers.checkedParse
import js7.data.value.expression.Expression.{Add, Multiply, NamedValue, NumericConstant}
import js7.data.value.expression.ExpressionParser.functionOnly
import js7.data.value.expression.scopes.NamedValueScope
import js7.data.value.{NamedValues, NumberValue, StringValue, Value}
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec

final class ExprFunctionTest extends AnyFreeSpec
{
  testEval("() => 1", Scope.empty,
    args = Nil,
    result = Right(NumberValue(1)),
    Right(ExprFunction(Nil, NumericConstant(1))))

  testEval("(a) => $a + 1", Scope.empty,
    args = Seq(NumberValue(2)),
    result = Right(NumberValue(3)),
    Right(ExprFunction(Seq(VariableDeclaration("a")), Add(NamedValue("a"), NumericConstant(1)))))

  testEval("(a, b) => $a + 2 * $b", Scope.empty,
    args = Seq(NumberValue(10), NumberValue(3)),
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
    args = Nil,
    result = Right(NumberValue(7)),
    Right(ExprFunction(Nil, NamedValue("nameFromContext"))))

  private def testEval(
    exprString: String,
    scope: Scope,
    args: Iterable[Value],
    result: Checked[Value],
    checkedFunction: Checked[ExprFunction])
    (implicit pos: source.Position): Unit =
    registerTest(exprString) {
      val checked = checkedParse(exprString, functionOnly(_))
      assert(checked == checkedFunction)
      for (function <- checkedFunction) {
        assert(checkedParse(function.toString, functionOnly(_)) == checkedFunction, " in toString❗")
        assert(function.eval(args)(scope) == result)
      }
    }

  "restrict" in {
    def restrict(min: Int, max: Int): Checked[ExprFunction] =
      for {
        function <- checkedParse("""(a, b) => "$a,$b"""", functionOnly(_))
        function <- function.restrict("myFunction", minimum = min, maximum = max)
      } yield function

    assert(restrict(1, 1) == Left(Problem(
      "The 'myFunction' function is expected to accept exactly 1 parameters")))

    assert(restrict(3, 4) == Left(Problem(
      "The 'myFunction' function is expected to accept between 3 and 4 parameters")))

    def eval(min: Int, max: Int, args: Iterable[String]): Checked[Value] =
      for {
        function <- restrict(min, max)
        result <- function.eval(args.map(StringValue(_)))(Scope.empty)
      } yield result

    assert(eval(1, 2, Nil) == Left(Problem(
      "Number of arguments=0 does not match required number of function parameters=1...2 in 'myFunction' function")))

    assert(eval(1, 2, Seq("A")) == Left(Problem("No such named value: b")))

    assert(eval(2, 2, Seq("A", "B")) ==
      Right(StringValue("A,B")))

    assert(eval(2, 2, Seq("A", "B", "C")) == Left(Problem(
    "Number of arguments=3 does not match required number of function parameters=2 in 'myFunction' function")))

    assert(eval(1, 2, Seq("A", "B", "C")) == Left(Problem(
    "Number of arguments=3 does not match required number of function parameters=1...2 in 'myFunction' function")))
  }
}
