package js7.data.value.expression

import fastparse.NoWhitespace._
import fastparse._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.value.expression.Expression._
import js7.data.value.expression.Scope.ConstantExpressionRequiredProblem
import js7.data.value.{BooleanValue, NumericValue, StringValue, Value}
import js7.data.workflow.Label
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.parser.Parsers.checkedParse
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks._

/**
  * @author Joacim Zschimmer
  */
final class EvaluatorTest extends AnyFreeSpec
{
  "NamedValue expressions" - {
    implicit val evaluator = new Evaluator(
      new Scope {
        private val symbols = Map[String, Value]("catchCount" -> NumericValue(3))
        val symbolToValue = name => symbols.checked(name)

        val findValue = {
          case ValueSearch(ValueSearch.LastOccurred, ValueSearch.NamedValue(name)) =>
            Right(Map("ASTRING" -> StringValue("AA"), "ANUMBER" -> NumericValue(7), "ABOOLEAN" -> BooleanValue(true), "returnCode" -> NumericValue(1)).get(name))
          case ValueSearch(ValueSearch.LastExecuted(PositionSearch.ByPrefix("PREFIX")), ValueSearch.NamedValue(name)) =>
            Right(Map("KEY" -> "LABEL-VALUE").get(name) map StringValue.apply)
          case ValueSearch(ValueSearch.LastExecuted(PositionSearch.ByLabel(Label("LABEL"))), ValueSearch.NamedValue(name)) =>
            Right(Map("KEY" -> StringValue("LABEL-VALUE"), "returnCode" -> NumericValue(2)).get(name))
          case ValueSearch(ValueSearch.LastExecuted(PositionSearch.ByWorkflowJob(WorkflowJob.Name("JOB"))), ValueSearch.NamedValue(name)) =>
            Right(Map("KEY" -> StringValue("JOB-VALUE"), "returnCode" -> NumericValue(3)).get(name))
          case ValueSearch(ValueSearch.Argument, ValueSearch.NamedValue(name)) =>
            Right(Map("ARG" -> "ARG-VALUE").get(name) map StringValue.apply)
          case o =>
            Left(Problem(s"UNEXPECTED CASE: $o"))
        }
      })
    val eval = evaluator.eval _
    val booleanError: BooleanExpression = LessThan(ToNumber(StringConstant("X")), NumericConstant(7))

    testEval("7",
      result = 7,
      Right(NumericConstant(7)))

    testEval(Int.MinValue.toString,  // -2147483648
      result = Int.MinValue,
      Right(NumericConstant(Int.MinValue)))

    testEval(""" "" """,
      result = "",
      Right(StringConstant("")))

    testEval( """ "\\" """,
      result = "\\",
      Right(StringConstant("\\")))

    testSyntaxError(""" "$var" """,
      """Expected properly terminated "-quoted string:1:2, found "$var\""""")

    testEval(""" "x" """,
      result = "x",
      Right(StringConstant("x")))

    testEval(""" 'a\x' """,
      result = "a\\x",
      Right(StringConstant("a\\x")))

    testEval("false",
      result = false,
      Right(BooleanConstant(false)))

    testEval("true",
      result = true,
      Right(BooleanConstant(true)))

    testEval("$ASTRING",
      result = "AA",
      Right(NamedValue.last("ASTRING")))

    testEval("${ASTRING}",
      result = "AA",
      Right(NamedValue.last("ASTRING")))

    //testEval("${label::LABEL.KEY}",
    //  result = "LABEL-VALUE",
    //  Right(NamedValue(NamedValue.ByLabel(Label("LABEL")), NamedValue.KeyValue(StringConstant("KEY")))))
    //
    //testEval("${job::JOB.KEY}",
    //  result = "JOB-VALUE",
    //  Right(NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), NamedValue.KeyValue(StringConstant("KEY")))))

    testEval("$UNKNOWN",
      result = Left(Problem("No such named value: UNKNOWN")),
      Right(NamedValue.last("UNKNOWN")))

    testEval("""variable("ASTRING")""",
      result = "AA",
      Right(NamedValue.last("ASTRING")))

    testEval("""variable(key="ASTRING")""",
      result = "AA",
      Right(NamedValue.last("ASTRING")))

    testEval("""variable("UNKNOWN")""",
      result = Left(Problem("No such named value: UNKNOWN")),
      Right(NamedValue.last("UNKNOWN")))

    testEval("""variable("UNKNOWN", default="DEFAULT")""",
      result = "DEFAULT",
      Right(NamedValue.last("UNKNOWN", StringConstant("DEFAULT"))))

    testEval("""variable(job=JOB, key="UNKNOWN", default="DEFAULT")""",
      result = "DEFAULT",
      Right(NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), NamedValue.KeyValue("UNKNOWN"), Some(StringConstant("DEFAULT")))))

    testEval("""variable(label=LABEL, key="UNKNOWN", default="DEFAULT")""",
      result = "DEFAULT",
      Right(NamedValue(NamedValue.ByLabel("LABEL"), NamedValue.KeyValue("UNKNOWN"), Some(StringConstant("DEFAULT")))))

    testEval("""argument("ARG")""",
      result = "ARG-VALUE",
      Right(NamedValue(NamedValue.Argument, NamedValue.KeyValue("ARG"))))

    testEval("""argument(key="ARG")""",
      result = "ARG-VALUE",
      Right(NamedValue(NamedValue.Argument, NamedValue.KeyValue("ARG"))))

    testEval("""argument("UNKNOWN", default="DEFAULT")""",
      result = "DEFAULT",
      Right(NamedValue(NamedValue.Argument, NamedValue.KeyValue("UNKNOWN"), Some(StringConstant("DEFAULT")))))

    testEval("""variable(key="returnCode", label=LABEL)""",
      result = 2,
      Right(NamedValue(NamedValue.ByLabel("LABEL"), NamedValue.KeyValue("returnCode"))))

    testEval("""variable(key="returnCode", job=JOB)""",
      result = 3,
      Right(NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), NamedValue.KeyValue("returnCode"))))

    testEval("""$ASTRING.toNumber""",
      result = Left(Problem("Not a valid number: AA")),
      Right(ToNumber(NamedValue.last("ASTRING"))))

    testEval("""$ANUMBER""",
      result = 7,
      Right(NamedValue.last("ANUMBER")))

    testEval(""""true".toBoolean""",
      result = true,
      Right(ToBoolean(StringConstant("true"))))

    testEval(""""false".toBoolean""",
      result = false,
      Right(ToBoolean(StringConstant("false"))))

    testEval(""" variable("ABOOLEAN")""",
      result = true,
      Right(NamedValue.last("ABOOLEAN")))

    locally {
      val longString =
         """LINE 1
           |LINE 2
            LINE 3
           |"""
      testEval(
        s"'$longString'.stripMargin",
        result = longString.stripMargin,
        Right(StripMargin(StringConstant(longString))))
    }

    testEval("""$returnCode == 0""",
      result = false,
      Right(Equal(LastReturnCode, NumericConstant(0))))

    testEval("""$returnCode >= 1""",
      result = true,
      Right(GreaterOrEqual(LastReturnCode, NumericConstant(1))))

    testEval("""$returnCode <= 1""",
      result = true,
      Right(LessOrEqual(LastReturnCode, NumericConstant(1))))

    testEval("""$returnCode > 1""",
      result = false,
      Right(GreaterThan(LastReturnCode, NumericConstant(1))))

    testEval("""$returnCode < 1""", false,
      Right(LessThan(LastReturnCode, NumericConstant(1))))

    testEval("""catchCount""",
      result = 3,
      Right(OrderCatchCount))

    testEval(""" "" matches "" """,
      result = true,
      Right(Matches(StringConstant(""), StringConstant(""))))

    testEval(""" "" matches "A.+" """,
      result = false,
      Right(Matches(StringConstant(""), StringConstant("A.+"))))

    testEval(""" "A" matches "A.+" """,
      result = false,
      Right(Matches(StringConstant("A"), StringConstant("A.+"))))

    testEval(""" "-A-" matches "A.+" """,
      result = false,
      Right(Matches(StringConstant("-A-"), StringConstant("A.+"))))

    testEval(""" "A--" matches "A.+" """,
      result = true,
      Right(Matches(StringConstant("A--"), StringConstant("A.+"))))

    testEval(""" "A-" matches "A.+" """,
      result = true,
      Right(Matches(StringConstant("A-"), StringConstant("A.+"))))

    testEval(""" variable("ASTRING") matches "A+" """,
      result = true,
      Right(Matches(NamedValue.last("ASTRING"), StringConstant("A+"))))

    testEval("!false",
      result = true,
      Right(Not(BooleanConstant(false))))

    testEval("! true",
      result = false,
      Right(Not(BooleanConstant(true))))

    testEval("!!true",
      result = true,
      Right(Not(Not(BooleanConstant(true)))))

    testEval("$returnCode >= 1 && !($returnCode <= 9) && $returnCode != 1",
      result = false,
      Right(
        And(
          And(
            GreaterOrEqual(LastReturnCode, NumericConstant(1)),
            Not(LessOrEqual(LastReturnCode, NumericConstant(9)))),
          NotEqual(LastReturnCode, NumericConstant(1)))))

    testEval("$returnCode in [1, 2, 3]",
      result = true,
      Right(
        In(LastReturnCode, ListExpression(List(NumericConstant(1), NumericConstant(2), NumericConstant(3))))))

    "Equal" in {
      forAll((a: Int, b: Int) => assert(
        eval(Equal(NumericConstant(a), NumericConstant(b))) == Right(BooleanValue(a == b))))
      assert(eval(Equal(NumericConstant(1), StringConstant("1"))) == Right(BooleanValue(false)))
    }

    "NotEqual" in {
      forAll((a: Int, b: Int) => assert(
        eval(NotEqual(NumericConstant(a), NumericConstant(b))) == Right(BooleanValue(a != b))))
      assert(eval(NotEqual(NumericConstant(1), StringConstant("1"))) == Right(BooleanValue(true)))
    }

    "LessOrEqual" in {
      forAll((a: Int, b: Int) => assert(
        eval(LessOrEqual(NumericConstant(a), NumericConstant(b))) == Right(BooleanValue(a <= b))))
    }

    "GreaterOrEqual" in {
      forAll((a: Int, b: Int) => assert(
        eval(GreaterOrEqual(NumericConstant(a), NumericConstant(b))) == Right(BooleanValue(a >= b))))
    }

    "LessThan" in {
      forAll((a: Int, b: Int) => assert(
        eval(LessThan(NumericConstant(a), NumericConstant(b))) == Right(BooleanValue(a < b))))
    }

    "GreaterThan" in {
      forAll((a: Int, b: Int) => assert(
        eval(GreaterThan(NumericConstant(a), NumericConstant(b))) == Right(BooleanValue(a > b))))
    }

    "In" in {
      forAll((a: Int, b: Int, c: Int, d: Int) => assert(
        eval(In(NumericConstant(a), ListExpression(NumericConstant(b) :: NumericConstant(c) :: NumericConstant(d) :: Nil)))
          == Right(BooleanValue(Set(b, c, d)(a)))))
    }

    "Not" in {
      forAll((bool: Boolean) => assert(
        eval(Not(BooleanConstant(bool))) == Right(BooleanValue(!bool))))
    }

    "And" in {
      forAll((a: Boolean, b: Boolean) => assert(
        eval(And(BooleanConstant(a), BooleanConstant(b))) == Right(BooleanValue(a && b))))
    }

    "And is lazy" in {
      assert(eval(And(BooleanConstant(true), booleanError)) == Left(Problem("Not a valid number: X")))
      assert(eval(And(BooleanConstant(false), booleanError)) == Right(BooleanValue(false)))
    }

    "Or" in {
      forAll((a: Boolean, b: Boolean) => assert(
        eval(Or(BooleanConstant(a), BooleanConstant(b))) == Right(BooleanValue(a || b))))
    }

    "Or is lazy" in {
      assert(eval(Or(BooleanConstant(true), booleanError)) == Right(BooleanValue(true)))
      assert(eval(Or(BooleanConstant(false), booleanError)) == Left(Problem("Not a valid number: X")))
    }

    "And and LessThan" in {
      assert(eval(And(LessThan(NumericConstant(1), NumericConstant(2)), LessThan(NumericConstant(1), ToNumber(StringConstant("7"))))) ==
        Right(BooleanValue(true)))
    }

    "mkString" in {
      assert(eval(MkString(ListExpression(StringConstant("»") :: NamedValue.last("ASTRING") :: NumericConstant(7) :: Nil)))
        == Right(StringValue("»AA7")))
    }
  }

  "Constant expressions" - {
    implicit val evaluator = Evaluator.Constant
    val eval = evaluator.eval _

    val longString =
       """LINE 1
         |LINE 2
          LINE 3
         |"""
    testEval(
      s"'$longString'.stripMargin",
      result = longString.stripMargin,
      Right(StripMargin(StringConstant(longString))))

    testEval("1 == 2",
      result = false,
      Right(Equal(NumericConstant(1), NumericConstant(2))))

    "Variables cannot be used" in {
      assert(eval(NamedValue.last("VARIABLE")) == Left(ConstantExpressionRequiredProblem))
    }
  }

  private def completeExpression[_: P] = ExpressionParser.expression ~ End

  private def testSyntaxError(exprString: String, problem: String)(implicit pos: source.Position): Unit =
    registerTest(s"$exprString - should fail") {
      assert(checkedParse(exprString.trim, completeExpression(_)) == Left(Problem(problem)))
    }

  private def testEval(exprString: String, result: Boolean, expression: Checked[Expression])(implicit evaluator: Evaluator, pos: source.Position): Unit =
    testEval(exprString, Right(BooleanValue(result)), expression)

  private def testEval(exprString: String, result: Int, expression: Checked[Expression])(implicit evaluator: Evaluator, pos: source.Position): Unit =
    testEval(exprString, Right(NumericValue(result)), expression)

  private def testEval(exprString: String, result: String, expression: Checked[Expression])(implicit evaluator: Evaluator, pos: source.Position): Unit =
    testEval(exprString, Right(StringValue(result)), expression)

  private def testEval(exprString: String, result: Checked[Value], expression: Checked[Expression])(implicit evaluator: Evaluator, pos: source.Position): Unit =
    registerTest(exprString) {
      assert(checkedParse(exprString.trim, completeExpression(_)) == expression)
      for (e <- expression) {
        assert(checkedParse(e.toString, completeExpression(_)) == expression, " *** toString ***")
        assert(evaluator.eval(e) == result)
      }
    }
}
