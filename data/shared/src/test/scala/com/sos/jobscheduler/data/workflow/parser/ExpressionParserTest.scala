package com.sos.jobscheduler.data.workflow.parser

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.expression.Expression
import com.sos.jobscheduler.data.expression.Expression._
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.parser.ExpressionParser._
import com.sos.jobscheduler.data.workflow.parser.Parsers.checkedParse
import fastparse.NoWhitespace._
import fastparse.{Parsed, _}
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExpressionParserTest extends AnyFreeSpec
{
  // See also EvaluatorTest

  "NamedValue" - {
    "$ with impossible names" in {
      assert(parse("$var/1", dollarNamedValue(_)) ==
        Parsed.Success(NamedValue.last("var"), 4))
      assert(parse("$var.1", dollarNamedValue(_)) ==
        Parsed.Success(NamedValue.last("var"), 4))
      assert(parse("$var-1", dollarNamedValue(_)) ==
        Parsed.Success(NamedValue.last("var"), 4))
      assert(parse("$var_1", dollarNamedValue(_)) ==
        Parsed.Success(NamedValue.last("var_1"), 6))
    }

    testExpression("""$key""", NamedValue.last("key"))
    testExpression("""$Schlüssel""", NamedValue.last("Schlüssel"))
    testExpression("""$clé""", NamedValue.last("clé"))
    testExpression("""$A""", NamedValue.last("A"))
    testExpression("""${SOME-KEY}""", NamedValue.last("SOME-KEY"))
    //testExpression("""${arg::SOME-KEY}""", NamedValue(NamedValue.Argument, NamedValue.KeyValue("SOME-KEY")))
    //testExpression("""${label::LABEL.SOME-KEY}""", NamedValue(NamedValue.ByLabel(Label("LABEL")), NamedValue.KeyValue("SOME-KEY")))
    //testExpression("""${job::JOB.SOME-KEY}""", NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), NamedValue.KeyValue("SOME-KEY")))
    //testExpression("""${A.SOME-KEY}""", NamedValue(NamedValue.LastOccurredByPrefix("A"), NamedValue.KeyValue("SOME-KEY")))

    "variable()" in {
      assert(checkedParse("""variable("clé")""", expression(_)) ==
        Right(NamedValue.last("clé")))
      assert(checkedParse("""variable ( "clé", default = "DEFAULT" )""", expression(_)) ==
        Right(NamedValue.last("clé", StringConstant("DEFAULT"))))
      assert(checkedParse("""variable(key="clé", label=LABEL)""", expression(_)) ==
        Right(NamedValue(NamedValue.ByLabel("LABEL"), NamedValue.KeyValue("clé"))))
      assert(checkedParse("""variable(key="clé", job=JOB)""", expression(_)) ==
        Right(NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), NamedValue.KeyValue("clé"))))
    }

    "argument()" in {
      assert(checkedParse("""argument("clé")""", expression(_)) ==
        Right(NamedValue(NamedValue.Argument, NamedValue.KeyValue("clé"))))
      assert(checkedParse("""argument ( "clé", default = "DEFAULT" )""", expression(_)) ==
        Right(NamedValue(NamedValue.Argument, NamedValue.KeyValue("clé"), Some(StringConstant("DEFAULT")))))
    }
  }

  "returnCode" - {
    testExpression("returnCode",
      LastReturnCode)
    testExpression("returnCode(label=LABEL)",
      NamedValue(NamedValue.ByLabel("LABEL"), NamedValue.ReturnCode))
    testExpression("returnCode(job=JOB)",
      NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), NamedValue.ReturnCode))
  }

  testExpression("catchCount", OrderCatchCount)

  "Boolean" - {
    testBooleanExpression("true", BooleanConstant(true))
    testBooleanExpression("false", BooleanConstant(false))
    testBooleanExpression("(false)", BooleanConstant(false))
  }

  "String" - {
    testExpression("'x'", StringConstant("x"))
    testExpression("'ö'", StringConstant("ö"))
    testExpression("""'a\x'""", StringConstant("""a\x"""))
    testExpression("""'a\\x'""", StringConstant("""a\\x"""))
    testExpression(""" "" """.trim, StringConstant(""))
    testExpression(""" "x" """.trim, StringConstant("x"))
    testExpression(""" "ö" """.trim, StringConstant("ö"))

    "Invalid strings" in {
      assert(checkedParse("''", expression(_)).isLeft)
      assert(checkedParse(""" "\" """.trim, expression(_)).isLeft)
      // We do not reject any string - assert(checkedParse(" \"\t\" ".trim, expression(_)).isLeft)
    }
  }

  //TODO testError(""""1" < 1""",
  //  """Expected Expression is not of type String: '1' < 1:1:8, found """"")

  "Comparison" - {
    testBooleanExpression("returnCode != 7",
      NotEqual(LastReturnCode, NumericConstant(7)))
    testBooleanExpression("returnCode > 7",
      GreaterThan(LastReturnCode, NumericConstant(7)))
    testBooleanExpression("""variable("A") == "X"""",
      Equal(NamedValue.last("A"), StringConstant("X")))
    testBooleanExpression("""$A == "X"""",
      Equal(NamedValue.last("A"), StringConstant("X")))

    testBooleanExpression("returnCode > 0 && returnCode < 9",
      And(
        GreaterThan(LastReturnCode, NumericConstant(0)),
        LessThan(LastReturnCode, NumericConstant(9))))

    testBooleanExpression("returnCode >= 0 && returnCode <= 9",
      And(
        GreaterOrEqual(LastReturnCode, NumericConstant(0)),
        LessOrEqual(LastReturnCode, NumericConstant(9))))

    testBooleanExpression("returnCode == 1 || returnCode == 2 || returnCode == 3",
      Or(
        Or(
          Equal(LastReturnCode, NumericConstant(1)),
          Equal(LastReturnCode, NumericConstant(2))),
        Equal(LastReturnCode, NumericConstant(3))))

    testBooleanExpression("""returnCode >= 0 && returnCode <= 9 && $result == "OK"""",
      And(
        And(
          GreaterOrEqual(LastReturnCode, NumericConstant(0)),
          LessOrEqual(LastReturnCode, NumericConstant(9))),
        Equal(NamedValue.last("result"), StringConstant("OK"))))

    testBooleanExpression("""returnCode in [0, 3, 50]""",
      In(
        LastReturnCode,
        ListExpression(List(NumericConstant(0), NumericConstant(3), NumericConstant(50)))))

    testError("""returnCode in [0, 3, 50] || $result == "1"""",
      """Expected boolean operarands for operator ||: [0, 3, 50] || $result == '1':1:43, found """"")

    testBooleanExpression("""(returnCode in [0, 3, 50]) || $result == "1"""",
      Or(
        In(
          LastReturnCode,
          ListExpression(List(NumericConstant(0), NumericConstant(3), NumericConstant(50)))),
        Equal(
          NamedValue.last("result"),
          StringConstant("1"))))

    testBooleanExpression("""returnCode==$expected.toNumber||$result=="1"||true&&returnCode>0""",
      Or(
        Or(
          Equal(
            LastReturnCode,
            ToNumber(NamedValue.last("expected"))),
          Equal(
            NamedValue.last("result"),
            StringConstant("1"))),
        And(
          BooleanConstant(true),
          GreaterThan(
            LastReturnCode,
            NumericConstant(0)))))

    testExpression("""["STRING", $NAME, 7].mkString""",
      MkString(ListExpression(StringConstant("STRING") :: NamedValue.last("NAME") :: NumericConstant(7) :: Nil)))
  }

  testExpression("'STRING'.stripMargin",
    StripMargin(StringConstant("STRING")))

  testBooleanExpression("""$result matches 'A.*'""",
    Matches(
      NamedValue.last("result"),
      StringConstant("A.*")))

  "Unknown numeric function" in {
    def parser[_: P] = expression ~ End
    assert(checkedParse(""""123".toNumber""", parser(_)) ==
      Right(ToNumber(StringConstant("123"))))
    assert(checkedParse(""""123".UNKNOWN""", parser(_)) ==
      Left(Problem("""Expected known function: .UNKNOWN:1:14, found """"")))
  }

  "Unknown boolean function" in {
    def parser[_: P] = expression ~ End
    assert(checkedParse(""""true".toBoolean""", parser(_)) ==
      Right(ToBoolean(StringConstant("true"))))
    assert(checkedParse(""""true".UNKNOWN""", parser(_)) ==
      Left(Problem("""Expected known function: .UNKNOWN:1:15, found """"")))
  }

  private def testBooleanExpression(exprString: String, expr: BooleanExpression)(implicit pos: source.Position) =
    registerTest(exprString) {
      def parser[_: P] = expression ~ End
      assert(checkedParse(exprString, parser(_)) == Right(expr))
      assert(checkedParse(expr.toString, parser(_)) == Right(expr), " - toString")
    }

  private def testExpression(exprString: String, expr: Expression)(implicit pos: source.Position) =
    registerTest(exprString) {
      def parser[_: P] = expression ~ End
      assert(checkedParse(exprString, parser(_)) == Right(expr))
      assert(checkedParse(expr.toString, parser(_)) == Right(expr), " - toString")
    }

  private def testError(exprString: String, errorMessage: String)(implicit pos: source.Position) =
    registerTest(exprString + " - should fail") {
      def parser[_: P] = expression ~ End
      assert(checkedParse(exprString, parser(_)) == Left(Problem(errorMessage)))
    }
}
