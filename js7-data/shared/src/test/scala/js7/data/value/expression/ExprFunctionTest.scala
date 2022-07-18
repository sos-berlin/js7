package js7.data.value.expression

import fastparse.NoWhitespace.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.parser.Parsers.checkedParse
import js7.data.value.ValueType.UnexpectedValueTypeProblem
import js7.data.value.expression.Expression.{Add, FunctionExpr, Multiply, NamedValue, NumericConstant}
import js7.data.value.expression.FastparseExpressionParser.{expressionOrFunction, functionOnly}
import js7.data.value.expression.scopes.NamedValueScope
import js7.data.value.{FunctionValue, NumberValue, StringValue, Value}
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
    scope = NamedValueScope("nameFromContext" -> NumberValue(7)),
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
        assert(checkedParse(exprString, expressionOrFunction(_)) == Right(FunctionExpr(function)))
      }
    }

  "FunctionExpr and FunctionValue" - {
    "() => 7" in {
      val fun: ExprFunction = ExprFunction(Nil, NumericConstant(7))
      val expr = checkedParse("() => 7", expressionOrFunction(_)).orThrow
      assert(expr == FunctionExpr(fun))
      implicit val scope = Scope.empty
      assert(expr.eval.orThrow.asInstanceOf[FunctionValue].function.eval(Nil) == Right(
        NumberValue(7)))
      assert(expr.eval.flatMap(_.asString) == Left(
        UnexpectedValueTypeProblem(StringValue, FunctionValue(fun))))
    }

    "(x) => x + 7" in {
      val expr = checkedParse("(x) => $x + 7", expressionOrFunction(_)).orThrow
      assert(expr == FunctionExpr(ExprFunction(
        Seq(VariableDeclaration("x")),
        Add(NamedValue("x"), NumericConstant(7)))))
      implicit val scope = Scope.empty
      assert(expr.eval.orThrow.asInstanceOf[FunctionValue].function.eval(NumberValue(10) :: Nil) ==
        Right(NumberValue(17)))
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
