package com.sos.jobscheduler.data.expression

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.data.expression.Evaluator.{BooleanValue, NumericValue, StringValue, Value}
import com.sos.jobscheduler.data.expression.Expression._
import com.sos.jobscheduler.data.expression.Scope.ConstantExpressionRequiredProblem
import com.sos.jobscheduler.data.workflow.Label
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.parser.ExpressionParser
import com.sos.jobscheduler.data.workflow.parser.Parsers.checkedParse
import fastparse.NoWhitespace._
import fastparse._
import org.scalactic.source
import org.scalatest.FreeSpec
import org.scalatest.prop.PropertyChecks._

/**
  * @author Joacim Zschimmer
  */
final class EvaluatorTest extends FreeSpec
{
  "NamedValue expressions" - {
    implicit val evaluator = new Evaluator(
      new Scope {
        private val symbols = Map[String, Value]("catchCount" -> NumericValue(3))
        val symbolToValue = name => symbols.checked(name)

        val findValue = {
          case ValueSearch(ValueSearch.LastOccurred, ValueSearch.KeyValue(key)) =>
            Valid(Map("ASTRING" -> "AA", "ANUMBER" -> "7", "ABOOLEAN" -> "true").get(key) map StringValue.apply)
          case ValueSearch(ValueSearch.LastExecuted(PositionSearch.ByPrefix("PREFIX")), ValueSearch.KeyValue(key)) =>
            Valid(Map("KEY" -> "LABEL-VALUE").get(key) map StringValue.apply)
          case ValueSearch(ValueSearch.LastExecuted(PositionSearch.ByLabel(Label("LABEL"))), ValueSearch.KeyValue(key)) =>
            Valid(Map("KEY" -> "LABEL-VALUE").get(key) map StringValue.apply)
          case ValueSearch(ValueSearch.LastExecuted(PositionSearch.ByWorkflowJob(WorkflowJob.Name("JOB"))), ValueSearch.KeyValue(key)) =>
            Valid(Map("KEY" -> "JOB-VALUE").get(key) map StringValue.apply)
          case ValueSearch(ValueSearch.LastOccurred, ValueSearch.ReturnCode) =>
            Valid(Some(NumericValue(1)))
          case ValueSearch(ValueSearch.LastExecuted(PositionSearch.ByLabel(Label("LABEL"))), ValueSearch.ReturnCode) =>
            Valid(Some(NumericValue(2)))
          case ValueSearch(ValueSearch.LastExecuted(PositionSearch.ByWorkflowJob(WorkflowJob.Name("JOB"))), ValueSearch.ReturnCode) =>
            Valid(Some(NumericValue(3)))
          case ValueSearch(ValueSearch.Argument, ValueSearch.KeyValue(key)) =>
            Valid(Map("ARG" -> "ARG-VALUE").get(key) map StringValue.apply)
        }
      })
    val eval = evaluator.eval _
    val booleanError: BooleanExpression = LessThan(ToNumber(StringConstant("X")), NumericConstant(7))

    testEval("7",
      result = 7,
      Valid(NumericConstant(7)))

    testEval(Int.MinValue.toString,  // -2147483648
      result = Int.MinValue,
      Valid(NumericConstant(Int.MinValue)))

    testEval(""" "" """,
      result = "",
      Valid(StringConstant("")))

    testEval( """ "\\" """,
      result = "\\",
      Valid(StringConstant("\\")))

    testSyntaxError(""" "$var" """,
      """Expected properly terminated "-quoted string:1:2, found "$var\""""")

    testEval(""" "x" """,
      result = "x",
      Valid(StringConstant("x")))

    testEval(""" 'a\x' """,
      result = "a\\x",
      Valid(StringConstant("a\\x")))

    testEval("false",
      result = false,
      Valid(BooleanConstant(false)))

    testEval("true",
      result = true,
      Valid(BooleanConstant(true)))

    testEval("$ASTRING",
      result = "AA",
      Valid(NamedValue.last("ASTRING")))

    testEval("${ASTRING}",
      result = "AA",
      Valid(NamedValue.last("ASTRING")))

    //testEval("${label::LABEL.KEY}",
    //  result = "LABEL-VALUE",
    //  Valid(NamedValue(NamedValue.ByLabel(Label("LABEL")), NamedValue.KeyValue(StringConstant("KEY")))))
    //
    //testEval("${job::JOB.KEY}",
    //  result = "JOB-VALUE",
    //  Valid(NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), NamedValue.KeyValue(StringConstant("KEY")))))

    testEval("$UNKNOWN",
      result = Invalid(Problem("No such named value: UNKNOWN")),
      Valid(NamedValue.last("UNKNOWN")))

    testEval("""variable("ASTRING")""",
      result = "AA",
      Valid(NamedValue.last("ASTRING")))

    testEval("""variable(key="ASTRING")""",
      result = "AA",
      Valid(NamedValue.last("ASTRING")))

    testEval("""variable("UNKNOWN")""",
      result = Invalid(Problem("No such named value: UNKNOWN")),
      Valid(NamedValue.last("UNKNOWN")))

    testEval("""variable("UNKNOWN", default="DEFAULT")""",
      result = "DEFAULT",
      Valid(NamedValue.last("UNKNOWN", StringConstant("DEFAULT"))))

    testEval("""variable(job=JOB, key="UNKNOWN", default="DEFAULT")""",
      result = "DEFAULT",
      Valid(NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), NamedValue.KeyValue("UNKNOWN"), Some(StringConstant("DEFAULT")))))

    testEval("""variable(label=LABEL, key="UNKNOWN", default="DEFAULT")""",
      result = "DEFAULT",
      Valid(NamedValue(NamedValue.ByLabel("LABEL"), NamedValue.KeyValue("UNKNOWN"), Some(StringConstant("DEFAULT")))))

    testEval("""argument("ARG")""",
      result = "ARG-VALUE",
      Valid(NamedValue(NamedValue.Argument, NamedValue.KeyValue("ARG"))))

    testEval("""argument(key="ARG")""",
      result = "ARG-VALUE",
      Valid(NamedValue(NamedValue.Argument, NamedValue.KeyValue("ARG"))))

    testEval("""argument("UNKNOWN", default="DEFAULT")""",
      result = "DEFAULT",
      Valid(NamedValue(NamedValue.Argument, NamedValue.KeyValue("UNKNOWN"), Some(StringConstant("DEFAULT")))))

    testEval("""returnCode""",
      result = 1,
      Valid(LastReturnCode))

    testEval("""returnCode(label=LABEL)""",
      result = 2,
      Valid(NamedValue(NamedValue.ByLabel("LABEL"), NamedValue.ReturnCode)))

    testEval("""returnCode(job=JOB)""",
      result = 3,
      Valid(NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), NamedValue.ReturnCode)))

    testEval("""$ASTRING.toNumber""",
      result = Invalid(Problem("Not a valid number: AA")),
      Valid(ToNumber(NamedValue.last("ASTRING"))))

    testEval("""$ANUMBER.toNumber""",
      result = 7,
      Valid(ToNumber(NamedValue.last("ANUMBER"))))

    testEval(""""true".toBoolean""",
      result = true,
      Valid(ToBoolean(StringConstant("true"))))

    testEval(""""false".toBoolean""",
      result = false,
      Valid(ToBoolean(StringConstant("false"))))

    testEval(""" variable("ABOOLEAN").toBoolean """,
      result = true,
      Valid(ToBoolean(NamedValue.last("ABOOLEAN"))))

    locally {
      val longString =
         """LINE 1
           |LINE 2
            LINE 3
           |"""
      testEval(
        s"'$longString'.stripMargin",
        result = longString.stripMargin,
        Valid(StripMargin(StringConstant(longString))))
    }

    testEval("""returnCode == 0""",
      result = false,
      Valid(Equal(LastReturnCode, NumericConstant(0))))

    testEval("""returnCode >= 1""",
      result = true,
      Valid(GreaterOrEqual(LastReturnCode, NumericConstant(1))))

    testEval("""returnCode <= 1""",
      result = true,
      Valid(LessOrEqual(LastReturnCode, NumericConstant(1))))

    testEval("""returnCode > 1""",
      result = false,
      Valid(GreaterThan(LastReturnCode, NumericConstant(1))))

    testEval("""returnCode < 1""", false,
      Valid(LessThan(LastReturnCode, NumericConstant(1))))

    testEval("""catchCount""",
      result = 3,
      Valid(OrderCatchCount))

    testEval(""" "" matches "" """,
      result = true,
      Valid(Matches(StringConstant(""), StringConstant(""))))

    testEval(""" "" matches "A.+" """,
      result = false,
      Valid(Matches(StringConstant(""), StringConstant("A.+"))))

    testEval(""" "A" matches "A.+" """,
      result = false,
      Valid(Matches(StringConstant("A"), StringConstant("A.+"))))

    testEval(""" "-A-" matches "A.+" """,
      result = false,
      Valid(Matches(StringConstant("-A-"), StringConstant("A.+"))))

    testEval(""" "A--" matches "A.+" """,
      result = true,
      Valid(Matches(StringConstant("A--"), StringConstant("A.+"))))

    testEval(""" "A-" matches "A.+" """,
      result = true,
      Valid(Matches(StringConstant("A-"), StringConstant("A.+"))))

    testEval(""" variable("ASTRING") matches "A+" """,
      result = true,
      Valid(Matches(NamedValue.last("ASTRING"), StringConstant("A+"))))

    testEval("!false",
      result = true,
      Valid(Not(BooleanConstant(false))))

    testEval("! true",
      result = false,
      Valid(Not(BooleanConstant(true))))

    testEval("!!true",
      result = true,
      Valid(Not(Not(BooleanConstant(true)))))

    testEval("returnCode >= 1 && !(returnCode <= 9) && returnCode != 1",
      result = false,
      Valid(
        And(
          And(
            GreaterOrEqual(LastReturnCode, NumericConstant(1)),
            Not(LessOrEqual(LastReturnCode, NumericConstant(9)))),
          NotEqual(LastReturnCode, NumericConstant(1)))))

    testEval("returnCode in [1, 2, 3]",
      result = true,
      Valid(
        In(LastReturnCode, ListExpression(List(NumericConstant(1), NumericConstant(2), NumericConstant(3))))))

    "Equal" in {
      forAll((a: Int, b: Int) => assert(
        eval(Equal(NumericConstant(a), NumericConstant(b))) == Valid(BooleanValue(a == b))))
      assert(eval(Equal(NumericConstant(1), StringConstant("1"))) == Valid(BooleanValue(false)))
    }

    "NotEqual" in {
      forAll((a: Int, b: Int) => assert(
        eval(NotEqual(NumericConstant(a), NumericConstant(b))) == Valid(BooleanValue(a != b))))
      assert(eval(NotEqual(NumericConstant(1), StringConstant("1"))) == Valid(BooleanValue(true)))
    }

    "LessOrEqual" in {
      forAll((a: Int, b: Int) => assert(
        eval(LessOrEqual(NumericConstant(a), NumericConstant(b))) == Valid(BooleanValue(a <= b))))
    }

    "GreaterOrEqual" in {
      forAll((a: Int, b: Int) => assert(
        eval(GreaterOrEqual(NumericConstant(a), NumericConstant(b))) == Valid(BooleanValue(a >= b))))
    }

    "LessThan" in {
      forAll((a: Int, b: Int) => assert(
        eval(LessThan(NumericConstant(a), NumericConstant(b))) == Valid(BooleanValue(a < b))))
    }

    "GreaterThan" in {
      forAll((a: Int, b: Int) => assert(
        eval(GreaterThan(NumericConstant(a), NumericConstant(b))) == Valid(BooleanValue(a > b))))
    }

    "In" in {
      forAll((a: Int, b: Int, c: Int, d: Int) => assert(
        eval(In(NumericConstant(a), ListExpression(NumericConstant(b) :: NumericConstant(c) :: NumericConstant(d) :: Nil)))
          == Valid(BooleanValue(Set(b, c, d)(a)))))
    }

    "Not" in {
      forAll((bool: Boolean) => assert(
        eval(Not(BooleanConstant(bool))) == Valid(BooleanValue(!bool))))
    }

    "And" in {
      forAll((a: Boolean, b: Boolean) => assert(
        eval(And(BooleanConstant(a), BooleanConstant(b))) == Valid(BooleanValue(a && b))))
    }

    "And is lazy" in {
      assert(eval(And(BooleanConstant(true), booleanError)) == Invalid(Problem("Not a valid number: X")))
      assert(eval(And(BooleanConstant(false), booleanError)) == Valid(BooleanValue(false)))
    }

    "Or" in {
      forAll((a: Boolean, b: Boolean) => assert(
        eval(Or(BooleanConstant(a), BooleanConstant(b))) == Valid(BooleanValue(a || b))))
    }

    "Or is lazy" in {
      assert(eval(Or(BooleanConstant(true), booleanError)) == Valid(BooleanValue(true)))
      assert(eval(Or(BooleanConstant(false), booleanError)) == Invalid(Problem("Not a valid number: X")))
    }

    "And and LessThan" in {
      assert(eval(And(LessThan(NumericConstant(1), NumericConstant(2)), LessThan(NumericConstant(1), ToNumber(StringConstant("7"))))) ==
        Valid(BooleanValue(true)))
    }

    "mkString" in {
      assert(eval(MkString(ListExpression(StringConstant("»") :: NamedValue.last("ASTRING") :: NumericConstant(7) :: Nil)))
        == Valid(StringValue("»AA7")))
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
      Valid(StripMargin(StringConstant(longString))))

    testEval("1 == 2",
      result = false,
      Valid(Equal(NumericConstant(1), NumericConstant(2))))

    "Variables cannot be used" in {
      assert(eval(NamedValue.last("VARIABLE")) == Invalid(ConstantExpressionRequiredProblem))
    }
  }

  private def completeExpression[_: P] = ExpressionParser.expression ~ End

  private def testSyntaxError(exprString: String, problem: String)(implicit evaluator: Evaluator, pos: source.Position): Unit =
    registerTest(s"$exprString - should fail") {
      assert(checkedParse(exprString.trim, completeExpression(_)) == Invalid(Problem(problem)))
    }

  private def testEval(exprString: String, result: Boolean, expression: Checked[Expression])(implicit evaluator: Evaluator, pos: source.Position): Unit =
    testEval(exprString, Valid(BooleanValue(result)), expression)

  private def testEval(exprString: String, result: Int, expression: Checked[Expression])(implicit evaluator: Evaluator, pos: source.Position): Unit =
    testEval(exprString, Valid(NumericValue(result)), expression)

  private def testEval(exprString: String, result: String, expression: Checked[Expression])(implicit evaluator: Evaluator, pos: source.Position): Unit =
    testEval(exprString, Valid(StringValue(result)), expression)

  private def testEval(exprString: String, result: Checked[Value], expression: Checked[Expression])(implicit evaluator: Evaluator, pos: source.Position): Unit =
    registerTest(exprString) {
      assert(checkedParse(exprString.trim, completeExpression(_)) == expression)
      for (e <- expression) {
        assert(checkedParse(e.toString, completeExpression(_)) == expression, " *** toString ***")
        assert(evaluator.eval(e) == result)
      }
    }
}
