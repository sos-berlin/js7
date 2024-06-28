package js7.data.value.expression

import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.data.job.JobResourcePath
import js7.data.value.Value.convenience.*
import js7.data.value.ValueType.{ErrorInExpressionProblem, UnexpectedValueTypeProblem}
import js7.data.value.expression.Expression.convenience.*
import js7.data.value.expression.Expression.{Divide, NamedValue, *}
import js7.data.value.expression.ExpressionParser.{parseExpression, parseExpressionOrFunction}
import js7.data.value.expression.scopes.NameToCheckedValueScope
import js7.data.value.{BooleanValue, ListValue, MissingValue, NumberValue, ObjectValue, StringValue, Value, missingValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Label
import org.scalactic.source
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks.{forAll, *}
import scala.collection.MapView

final class ExpressionTest extends OurTestSuite
{
  implicit val scope: Scope =
    new Scope {

      import PositionSearch.{ByLabel, ByPrefix, ByWorkflowJob}
      import ValueSearch.{LastExecuted, Name}

      override def symbolToValue(symbol: String) =
        symbol match {
          case "catchCount" => Some(Right(3))
          case _ => None
        }

      override lazy val nameToCheckedValue =
        MapView(
          "ASTRING" -> Right(StringValue("AA")),
          "ANUMBER" -> Right(7),
          "ABOOLEAN" -> Right(BooleanValue(true)),
          "WEIRD/NAME" -> Right(StringValue("weird")),
          "returnCode" -> Right(1),
          "myObject" -> Right(ObjectValue(Map(
            "myField" -> ObjectValue(Map(
              "a" -> 1))))),
          "missing" -> Right(MissingValue))

      override def findValue(search: ValueSearch) =
        search match {
          case ValueSearch(ValueSearch.LastOccurred, Name(name)) =>
            nameToCheckedValue.get(name)

          case ValueSearch(LastExecuted(ByPrefix("PREFIX")), Name(name)) =>
            Map("KEY" -> "LABEL-VALUE")
              .get(name)
              .map(o => Right(StringValue(o)))

          case ValueSearch(LastExecuted(ByLabel(Label("LABEL"))), Name(name)) =>
            Map(
              "KEY" -> StringValue("LABEL-VALUE"),
              "returnCode" -> NumberValue(2)
            ).get(name).map(Right(_))

          case ValueSearch(LastExecuted(ByWorkflowJob(WorkflowJob.Name("JOB"))), Name(name)) =>
            Map(
              "KEY" -> StringValue("JOB-VALUE"),
              "returnCode" -> NumberValue(3)
            ).get(name).map(Right(_))

          case ValueSearch(ValueSearch.Argument, Name(name)) =>
            Map("ARG" -> "ARG-VALUE")
              .get(name)
              .map(o => Right(StringValue(o)))

          case _ =>
            None
        }

      override def evalFunctionCall(functionCall: FunctionCall)(implicit scope: Scope) =
        functionCall match {
          case FunctionCall("myFunction", Seq(Argument(expr, None))) =>
            Some(
              for {
                value <- expr.eval
                maybeNumber <- value.asMaybeNumber
              } yield maybeNumber.map(_ * 3).fold(missingValue)(NumberValue(_)))

          case _ => None
        }

      override def evalJobResourceVariable(v: JobResourceVariable)(implicit scope: Scope) =
        v match {
          case JobResourceVariable(JobResourcePath("myJobResource"), Some("VARIABLE")) =>
            Some(Right(StringValue("myJobResource,VARIABLE,value")))

          case JobResourceVariable(JobResourcePath("JOB-RESOURCE"), Some("VARIABLE-NAME")) =>
            Some(Right(StringValue("JOB-RESOURCE,VARIABLE-NAME,value")))

          case _ => None
        }
    }

  "Constants" - {
    "Numbers" - {
      testEval("7",
        result = Right(7),
        7)

      testEval(Long.MinValue.toString,
        result = Right(Long.MinValue),
        Long.MinValue)

      testEval("-1.111222333444555666777888999",
        result = Right(BigDecimal("-1.111222333444555666777888999")),
        BigDecimal("-1.111222333444555666777888999"))

      locally {
        val number = "-111222333444555666777888999000111222333444555666777888999000"
        testEval(number,
          result = Right(BigDecimal(number)),
          BigDecimal(number))
      }
    }

    "Strings" - {
      testEval(""" "" """,
        result = Right(""),
        "")

      testEval(""" "x" """,
        result = Right("x"),
        "x")

      testEval(""" 'a\x' """,
        result = Right("a\\x"),
        "a\\x")

      testEval( """ "\\" """,
        result = Right("\\"),
        "\\")
    }

    "Boolean values" - {
      testEval("false",
        result = Right(false),
        false)

      testEval("true",
        result = Right(true),
        true)
    }

    "Objects" - {
      testEval("{a: 'AAA', missing: missing}.a",
        result = Right("AAA"),
        DotExpr(ObjectExpr(Map("a" -> "AAA", "missing" -> MissingConstant)), "a"))
    }

    "Lists" - {
      testEval("[ 'AAA', missing, 7 ]",
        result = Right(ListValue(Seq("AAA", MissingValue, 7))),
        ListExpr(List("AAA", MissingConstant, 7)))

      testEval("""[ 'AAA', error("ERROR"), 7 ]""",
        result = Left(ErrorInExpressionProblem("ERROR")),
        ListExpr(List("AAA", ErrorExpr("ERROR"), 7)))
    }
  }

  "Multi-line strings" - {
    locally {
      val longString =
        """LINE 1
   |LINE 2
    LINE 3
   |"""
      testEval(
        s"'$longString'.stripMargin",
        result = Right(longString.stripMargin),
        StripMargin(longString))
    }
  }

  "String interpolation" - {
    testEval(""""-->$ASTRING${ABOOLEAN}$(missing)$(100 + $ANUMBER)<--"""",
      result = Right("-->AAtrue107<--"),
      InterpolatedString(List(
        "-->",
        NamedValue("ASTRING"),
        NamedValue("ABOOLEAN"),
        MissingConstant,
        Add(100, NamedValue("ANUMBER")),
        "<--")))
  }

  "Variable reference via $" - {
    testEval("$ASTRING",
      result = Right("AA"),
      NamedValue("ASTRING"))

    testEval("${ASTRING}",
      result = Right("AA"),
      NamedValue("ASTRING"))

    testEval("$`WEIRD/NAME`",
      result = Right("weird"),
      NamedValue("WEIRD/NAME"))

    testEval("""$ANUMBER""",
      result = Right(7),
      NamedValue("ANUMBER"))

    testEval("$UNKNOWN",
      result = Left(Problem("No such named value: UNKNOWN")),
      NamedValue("UNKNOWN"))

    testEval("""$myObject""",
      result = Right(ObjectValue(Map("myField" -> ObjectValue(Map("a" -> 1))))),
      NamedValue("myObject"))
  }

  //testEval("${label::LABEL.KEY}",
  //  result = Right("LABEL-VALUE"),
  //  Right(NamedValue(NamedValue.ByLabel(Label("LABEL")), ("KEY"))))
  //
  //testEval("${job::JOB.KEY}",
  //  result = Right("JOB-VALUE"),
  //  Right(NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), ("KEY"))))

  "JobResource" - {
    testEval("JobResource:myJobResource:VARIABLE",
      result = Right("myJobResource,VARIABLE,value"),
      JobResourceVariable(JobResourcePath("myJobResource"), Some("VARIABLE")))

    testEval("JobResource:JOB-RESOURCE:`VARIABLE-NAME`",
      result = Right("JOB-RESOURCE,VARIABLE-NAME,value"),
      JobResourceVariable(JobResourcePath("JOB-RESOURCE"), Some("VARIABLE-NAME")))
  }

  testEval("""catchCount""",
    result = Right(3),
    OrderCatchCount)

  "variable()" - {
    testEval("""variable("ASTRING")""",
      result = Right("AA"),
      NamedValue("ASTRING"))

    testEval("""variable(missing)""",
      result = Right(MissingValue),
      NamedValue(NamedValue.LastOccurred, MissingConstant))

    testEval("""variable(key="ASTRING")""",
      result = Right("AA"),
      NamedValue("ASTRING"))

    testEval("""variable("UNKNOWN")""",
      result = Left(Problem("No such named value: UNKNOWN")),
      NamedValue("UNKNOWN"))

    testEval("""variable("UNKNOWN", default="DEFAULT")""",
      result = Right("DEFAULT"),
      NamedValue("UNKNOWN", "DEFAULT"))

    testEval("""variable(job=JOB, key="UNKNOWN", default="DEFAULT")""",
      result = Right("DEFAULT"),
      NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), "UNKNOWN", Some("DEFAULT")))

    testEval("""variable(label=LABEL, key="UNKNOWN", default="DEFAULT")""",
      result = Right("DEFAULT"),
      NamedValue(NamedValue.ByLabel("LABEL"), "UNKNOWN", Some("DEFAULT")))

    testEval("""variable(key="returnCode", label=LABEL)""",
      result = Right(2),
      NamedValue(NamedValue.ByLabel("LABEL"), "returnCode"))

    testEval("""variable(key="returnCode", job=JOB)""",
      result = Right(3),
      NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), "returnCode"))

    testEval(""" variable("ABOOLEAN")""",
      result = Right(true),
      NamedValue("ABOOLEAN"))
  }

  "argument()" - {
    testEval("""argument("ARG")""",
      result = Right("ARG-VALUE"),
      NamedValue(NamedValue.Argument, "ARG"))

    testEval("""argument(missing)""",
      result = Right(MissingValue),
      NamedValue(NamedValue.Argument, MissingConstant))

    testEval("""argument(key="ARG")""",
      result = Right("ARG-VALUE"),
      NamedValue(NamedValue.Argument, "ARG"))

    testEval("""argument("UNKNOWN", default="DEFAULT")""",
      result = Right("DEFAULT"),
      NamedValue(NamedValue.Argument, "UNKNOWN", Some("DEFAULT")))
  }

  "toBoolean()" - {
    testEval("""toBoolean('true')""",
      result = Right(true),
      ToBoolean("true"))

    testEval("""toBoolean('false')""",
      result = Right(false),
      ToBoolean("false"))

    testEval("""toBoolean(missing)""",
      result = Right(MissingValue),
      ToBoolean(MissingConstant))

    testEval(""""true".toBoolean /*deprecated*/""",
      result = Right(true),
      ToBoolean("true"))

    testEval(""""false".toBoolean /*deprecated*/""",
      result = Right(false),
      ToBoolean("false"))
  }

  "toNumber()" - {
    testEval("""toNumber($ASTRING)""",
      result = Left(Problem("Not a valid number: AA")),
      ToNumber(NamedValue("ASTRING")))

    testEval("""toNumber('123')""",
      result = Right(123),
      ToNumber("123"))

    testEval("""toNumber(missing)""",
      result = Right(MissingValue),
      ToNumber(MissingConstant))

    // Deprecated
    testEval("""$ASTRING.toNumber /*deprecated*/""",
      result = Left(Problem("Not a valid number: AA")),
      ToNumber(NamedValue("ASTRING")))
  }

  "mkString()" - {
    testEval("""mkString(123)""",
      result = Right("123"),
      MkString(123))

    testEval("""mkString(missing)""",
      result = Right(""),
      MkString(MissingConstant))

    testEval("""mkString([$ANUMBER, "-", missing, true])""",
      result = Right("7-true"),
      MkString(ListExpr(List(
        NamedValue("ANUMBER"),
        "-",
        MissingConstant,
        true))))

    testEval("""mkString([ "-->", error("ERROR") ])""",
      result = Left(ErrorInExpressionProblem("ERROR"))/*because the list fails*/,
      MkString(ListExpr(List(
        "-->",
        ErrorExpr("ERROR")))))
  }

  "stripMargin()" - {
    testEval("stripMargin('  |ONE\n  |TWO')",
      result = Right("ONE\nTWO"),
      StripMargin("  |ONE\n  |TWO"))

    testEval("stripMargin(missing)",
      result = Right(MissingValue),
      StripMargin(MissingConstant))
  }

  "Dot operator" - {
    testEval("""3 + $myObject.myField.a""",
      result = Right(4),
      Add(
        3,
        DotExpr(DotExpr(NamedValue("myObject"), "myField"), "a")))

    testEval(""""$($myObject.myField.a)"""",
      result = Right("1"),
      InterpolatedString(List(
        DotExpr(DotExpr(NamedValue("myObject"), "myField"), "a"))))

    testEval(""""$ANUMBER-$($myObject.myField.a)"""",
      result = Right("7-1"),
      InterpolatedString(List(
        NamedValue("ANUMBER"),
        "-",
        DotExpr(DotExpr(NamedValue("myObject"), "myField"), "a"))))

    testEval(""""${myObject.myField.a}"""",
      result = Right("1"),
      InterpolatedString(List(
        DotExpr(DotExpr(NamedValue("myObject"), "myField"), "a"))))
  }

  "function expression call" in {
    pending
  }

  "Operators" - {
    "?" - {
      testEval("error('ERROR') ?",
        result = Right(MissingValue),
        OrMissing(ErrorExpr("ERROR")))

      testEval("error('ERROR') ? 7 + 3",
        result = Right(7 + 3),
        Add(
          OrElse(ErrorExpr("ERROR"), 7),
          3))

      testEval("$unknown ? 7 + 3",
        result = Right(7 + 3),
        Add(
          OrElse(NamedValue("unknown"), 7),
          3))

      testEval("1 + 2 ? 7 + 3",
        result = Right(1 + 2 + 3),
        Add(
          Add(
            1,
            OrElse(2, 7)),
          3))

      testEval("1 + (2 ? 7) + 3",
        result = Right(1 + 2 + 3),
        Add(
          Add(1, OrElse(2, 7)),
          3))

      testEval("""error('ERROR')? ?""",
        result = Right(MissingValue),
        OrMissing(OrMissing(ErrorExpr("ERROR"))))

      testEval("""(error('ERROR')?)?""",
        result = Right(MissingValue),
        OrMissing(OrMissing(ErrorExpr("ERROR"))))

      testEval("""error('ERROR')? ? 7""",
        result = Right(7),
        OrElse(OrMissing(ErrorExpr("ERROR")), 7))

      testEval("""error('ERROR')? ? ? 7""",
        result = Right(7),
        OrElse(OrMissing(OrMissing(ErrorExpr("ERROR"))), 7))

      // Reserve »??« for future use
      testSyntaxError("""(1/0) ??""", Problem(
        "Error in expression: Parsing failed at position 8 “(1/0) ?❓?” · Unexpected “?”"))

      testSyntaxError("""(1/0) ?? -1""", Problem(
        "Error in expression: Parsing failed at position 8 “(1/0) ?❓? -1” · Unexpected “?”"))

      testEval("missing?",
        result = Right(MissingValue),
        OrMissing(MissingConstant))

      testEval("missing ? 7 + 3",
        result = Right(7 + 3),
        Add(
          OrElse(MissingConstant, 7),
          3))

      testEval("""6 / 3?""",
        result = Right(2),
        Divide(6, OrMissing(3)))

      testEval("""(1 / 0)?""",
        result = Right(MissingValue),
        OrMissing(Divide(1, 0)))

      testEval("""(1 / 0) ? -1""",
        result = Right(-1),
        OrElse(Divide(1, 0), -1))

      testEval("""(7 in [ 1 / 0 ]) ? $unknown ? -1""",
        result = Right(-1),
        OrElse(
          OrElse(
            In(
              7,
              ListExpr(List(Divide(1, 0)))),
            NamedValue("unknown")),
          -1))

      testEval("""$aUnknown ? $bUnknown ? """,
        result = Right(MissingValue),
        OrMissing(
          OrElse(
            NamedValue("aUnknown"),
            NamedValue("bUnknown"))))
    }

    "==" - {
      testEval("""$returnCode == 1""",
        result = Right(true),
        Equal(LastReturnCode, 1))

      testEval("""$returnCode == 0""",
        result = Right(false),
        Equal(LastReturnCode, 0))

      testEval("""'1' == 1""",
        result = Right(false),
        Equal("1", 1))

      testEval("""missing == 1""",
        result = Right(false),
        Equal(MissingConstant, 1))

      testEval("""missing == missing""",
        result = Right(true),
        Equal(MissingConstant, MissingConstant))

      testEval("""error("ERROR") == error("ERROR")""",
        result = Left(ErrorInExpressionProblem("ERROR")),
        Equal(ErrorExpr("ERROR"), ErrorExpr("ERROR")))

      testEval("""error("ERROR") == error("X")""",
        result = Left(ErrorInExpressionProblem("ERROR")),
        Equal(ErrorExpr("ERROR"), ErrorExpr("X")))

      "Equal" in {
        forAll((a: Int, b: Int) => assert(
          Equal(a, b).eval == Right(BooleanValue(a == b))))
        assert(Equal(1, "1").eval == Right(BooleanValue(false)))
      }
    }

    "!=" - {
      testEval("""$returnCode != 1""",
        result = Right(false),
        NotEqual(LastReturnCode, 1))

      testEval("""$returnCode != 0""",
        result = Right(true),
        NotEqual(LastReturnCode, 0))

      testEval("""'1' != 1""",
        result = Right(true),
        NotEqual("1", 1))

      testEval("""missing != 1""",
        result = Right(true),
        NotEqual(MissingConstant, 1))

      testEval("""missing != missing""",
        result = Right(false),
        NotEqual(MissingConstant, MissingConstant))

      testEval("""error("ERROR") != error("ERROR")""",
        result = Left(ErrorInExpressionProblem("ERROR")),
        NotEqual(ErrorExpr("ERROR"), ErrorExpr("ERROR")))

      testEval("""error("ERROR") != error("X")""",
        result = Left(ErrorInExpressionProblem("ERROR")),
        NotEqual(ErrorExpr("ERROR"), ErrorExpr("X")))

      "NotEqual" in {
        forAll((a: Int, b: Int) => assert(
          NotEqual(a, b).eval == Right(BooleanValue(a != b))))
      }
    }

    "<=" - {
      testEval("""$returnCode <= 1""",
        result = Right(true),
        LessOrEqual(LastReturnCode, 1))

      testEval("""missing <= 1""",
        result = Right(MissingValue),
        LessOrEqual(MissingConstant, 1))

      "LessOrEqual" in {
        forAll((a: Int, b: Int) => assert(
          LessOrEqual(a, b).eval == Right(BooleanValue(a <= b))))
      }
    }

    "<" - {
      testEval("""$returnCode < 1""",
        result = Right(false),
        LessThan(LastReturnCode, 1))

      testEval("""missing < 1""",
        result = Right(MissingValue),
        LessThan(MissingConstant, 1))

      testEval("""$returnCode * 3 == 3""",
        result = Right(true),
        Equal(Multiply(LastReturnCode, 3), 3))

      "LessThan" in {
        forAll((a: Int, b: Int) => assert(
          LessThan(a, b).eval == Right(BooleanValue(a < b))))
      }
    }

    ">=" - {
      testEval("""$returnCode >= 1""",
        result = Right(true),
        GreaterOrEqual(LastReturnCode, 1))

      testEval("""missing >= 1""",
        result = Right(MissingValue),
        GreaterOrEqual(MissingConstant, 1))

      "GreaterOrEqual" in {
        forAll((a: Int, b: Int) => assert(
          GreaterOrEqual(a, b).eval == Right(BooleanValue(a >= b))))
      }
    }

    ">" - {
      testEval("""$returnCode > 1""",
        result = Right(false),
        GreaterThan(LastReturnCode, 1))

      testEval("""missing > 1""",
        result = Right(MissingValue),
        GreaterThan(MissingConstant, 1))

      "GreaterThan" in {
        forAll((a: Int, b: Int) => assert(
          GreaterThan(a, b).eval == Right(BooleanValue(a > b))))
      }
    }

    "*" - {
      testEval("""3 * 7""",
        result = Right(BigDecimal(21)),
        Multiply(3, 7))

      testEval("""-3 * -7""",
        result = Right(BigDecimal(21)),
        Multiply(-3, -7))

      testEval("""3.1 * -7.1""",
        result = Right(BigDecimal("-22.01")),
        Multiply(BigDecimal("3.1"), BigDecimal("-7.1")))

      testEval("""missing * 7""",
        result = Right(MissingValue),
        Multiply(MissingConstant, BigDecimal(7)))

      testEval("""7 * missing""",
        result = Right(MissingValue),
        Multiply(BigDecimal(7), MissingConstant))

      testEval("""error("ERROR") * missing""",
        result = Left(ErrorInExpressionProblem("ERROR")),
        Multiply(ErrorExpr("ERROR"), MissingConstant))

      testEval("""missing * error("ERROR")""",
        result = Left(ErrorInExpressionProblem("ERROR")),
        Multiply(MissingConstant, ErrorExpr("ERROR")))
    }

    "/" - {
      testEval("""1 / 3""",
        result = Right(BigDecimal("0.3333333333333333333333333333333333")),
        Divide(1, 3))

      testEval("""6 / -2""",
        result = Right(BigDecimal(-3)),
        Divide(6, -2))

      testEval("""-6 / -2""",
        result = Right(BigDecimal(3)),
        Divide(-6, -2))

      testEval("""1 / 0""",
        result = Left(Problem("ArithmeticException: Division by zero")),
        Divide(1, 0))

      testEval("""(1 / 0)?""",
        result = Right(MissingValue),
        OrMissing(Divide(1, 0)))

      testEval("""missing / 1""",
        result = Right(MissingValue),
        Divide(MissingConstant, 1))

      testEval("""1 / missing""",
        result = Right(MissingValue),
        Divide(1, MissingConstant))

      testEval("""(1/0) / missing""",
        result = Left(Problem("ArithmeticException: Division by zero")),
        Divide(Divide(1, 0), MissingConstant))

      // Operation is not lazy
      testEval("""missing / (1/0)""",
        result = Left(Problem("ArithmeticException: Division by zero")),
        Divide(MissingConstant, Divide(1, 0)))
    }

    "+" - {
      testEval("""$returnCode + -3""",
        result = Right(-2),
        Add(LastReturnCode, -3))

      testEval("""$returnCode + "3"""",
        result = Left(UnexpectedValueTypeProblem(NumberValue, StringValue("3"))),
        Add(LastReturnCode, "3"))

      testEval("""$returnCode + missing""",
        result = Right(MissingValue),
        Add(LastReturnCode, MissingConstant))

      testEval("""missing + $returnCode""",
        result = Right(MissingValue),
        Add(MissingConstant, LastReturnCode))

      testEval("""1 / 0 + missing""",
        result = Left(Problem("ArithmeticException: Division by zero")),
        Add(Divide(1, 0), MissingConstant))

      // Operation is not lazy
      testEval("""missing + 1 / 0""",
        result = Left(Problem("ArithmeticException: Division by zero")),
        Add(MissingConstant, Divide(1, 0)))
    }

    "-" - {
      testEval("""$returnCode - 3""",
        result = Right(-2),
        Substract(LastReturnCode, 3))

      testEval("""$returnCode - "3"""",
        result = Left(UnexpectedValueTypeProblem(NumberValue, StringValue("3"))),
        Substract(LastReturnCode, "3"))

      testEval("""$returnCode - missing""",
        result = Right(MissingValue),
        Substract(LastReturnCode, MissingConstant))

      testEval("""missing - $returnCode""",
        result = Right(MissingValue),
        Substract(MissingConstant, LastReturnCode))

      testEval("""1 / 0 - missing""",
        result = Left(Problem("ArithmeticException: Division by zero")),
        Substract(Divide(1, 0), MissingConstant))

      // Operation is not lazy
      testEval("""missing - 1 / 0""",
        result = Left(Problem("ArithmeticException: Division by zero")),
        Substract(MissingConstant, Divide(1, 0)))
    }

    "++" - {
      testEval("""'->' ++ $returnCode ++ '<-'""",
        result = Right("->1<-"),
        Concat(Concat("->", LastReturnCode), "<-"))

      // Use string interpolation when missing should evaluate to the empty string.
      testEval("""'->' ++ missing ++ '<-'""",
        result = Right(MissingValue),
        Concat(Concat("->", MissingConstant), "<-"))

      // ErrorValue has priority over MissingValue
      testEval("""missing ++ error("ERROR") ++ missing""",
        result = Left(ErrorInExpressionProblem("ERROR")),
        Concat(Concat(MissingConstant, ErrorExpr("ERROR")), MissingConstant))
    }
  }

  "!" - {
    testEval("!false",
      result = Right(true),
      Not(false))

    testEval("! true",
      result = Right(false),
      Not(true))

    testEval("!!true",
      result = Right(true),
      Not(Not(true)))

    "Not" in {
      forAll((bool: Boolean) => assert(
        Not(bool).eval == Right(BooleanValue(!bool))))
    }
  }

  "function call" - {
    testEval(""" myFunction(7) """,
      result = Right(21),
      FunctionCall("myFunction", Seq(Argument(7))))
  }

  "objectExpression" in {
    val expr = Map[String, Expression](
      "A" -> 1,
      "B" -> "BBB",
      "LIST" -> ListExpr(List(1, 2, 3)))
    assert(scope.evalExpressionMap(expr) ==
      Right(Map[String, Value](
        "A" -> 1,
        "B" -> "BBB",
        "LIST" -> ListValue(List[Value](1, 2, 3)))),
      In(LastReturnCode, ListExpr(List(1, 2, 3))))
  }

  //testEval("""{"A": 1, "B": "BBB", "LIST": [1, 2, 3]}""",
  //  result = Right(ObjectValue(Map(
  //    "A" -> (1),
  //    "B" -> StringValue("BBB"),
  //    "LIST" -> ListValue(List((1), (2), (3)))))),
  //  In(LastReturnCode, ListExpr(List((1), (2), (3)))))

  "Operator precedence" - {
    testEval("""true || false && 3 == 4 < 5 + 6 * 7 ? 8""",
      result = Right(BooleanValue(true)),
      Or(true, And(false, Equal(3, LessThan(4, Add(5, Multiply(6, OrElse(7, 8))))))))
  }

  "replaceAll" - {
    testEval(""" replaceAll("abcdef", "([ae])", '»$1«') """,
      result = Right("»a«bcd»e«f"),
      ReplaceAll("abcdef", "([ae])", "»$1«"))

    testEval(""" replaceAll(missing, "([ae])", '»$1«') """,
      result = Right(MissingValue),
      ReplaceAll(MissingConstant, "([ae])", "»$1«"))

    testEval(""" replaceAll("abcdef", missing, '»$1«') """,
      result = Right(MissingValue),
      ReplaceAll("abcdef", MissingConstant, "»$1«"))

    testEval(""" replaceAll("abcdef", "([ae])", missing) """,
      result = Right(MissingValue),
      ReplaceAll("abcdef", "([ae])", MissingConstant))
  }

  "mkString" - {
    testEval(""" mkString(["»", $ASTRING, 7]) """,
      result = Right(StringValue("»AA7")),
      MkString(ListExpr(List("»", NamedValue("ASTRING"), 7))))

    // Fails because List construction fails
    testEval(""" mkString(["»", missing, error("ERROR"), 7]) """,
      result = Left(ErrorInExpressionProblem("ERROR")),
      MkString(ListExpr(List("»", MissingConstant, ErrorExpr("ERROR"), 7))))
  }

  "error(\"ERROR\")" - {
    implicit val scope = Scope.empty

    testEval("error('ERROR')",
      result = Left(ErrorInExpressionProblem("ERROR")),
      ErrorExpr("ERROR"))

    testEval("error('ERROR')?",
      result = Right(MissingValue),
      OrMissing(ErrorExpr("ERROR")))

    testEval("error('ERROR') ?",
      result = Right(MissingValue),
      OrMissing(ErrorExpr("ERROR")))

    testEval("error('ERROR') == error('ERROR')",
      result = Left(ErrorInExpressionProblem("ERROR")),
      Equal(ErrorExpr("ERROR"), ErrorExpr("ERROR")))

    testEval("error('ERROR') != error('ERROR')",
      result = Left(ErrorInExpressionProblem("ERROR")),
      NotEqual(ErrorExpr("ERROR"), ErrorExpr("ERROR")))

    testEval("error('ERROR') + 1",
      result = Left(ErrorInExpressionProblem("ERROR")),
      Add(ErrorExpr("ERROR"), 1))
  }

  "MissingValue OrMissing" in {
    assert(OrMissing(ErrorExpr("ERROR")).eval == Right(MissingValue))
  }

  "MissingValue is not comparable" in {
    assert(Equal(ErrorExpr("ERROR"), ErrorExpr("ERROR")).eval ==
      Left(ErrorInExpressionProblem("ERROR")))
  }

  testEval("\"-->$(error('ERROR'))<--\"",
    result = Left(ErrorInExpressionProblem("ERROR")),
    InterpolatedString(List("-->", ErrorExpr("ERROR"), "<--")))

  testEval("\"-->$(error('ERROR')?)<--\"",
    result = Right(StringValue("--><--")),
    InterpolatedString(List("-->", OrMissing(ErrorExpr("ERROR")), "<--")))

  "Missing value" - {
    implicit val scope = Scope.empty

    testEval("missing",
      result = Right(MissingValue),
      MissingConstant)

    testEval("missing == missing",
      result = Right(true),
      Equal(MissingConstant, MissingConstant))

    testEval("missing != missing",
      result = Right(false),
      NotEqual(MissingConstant, MissingConstant))

    testEval("missing + 1",
      result = Right(MissingValue),
      Add(MissingConstant, 1))

    testEval("\"-->$(missing)<--\"",
      result = Right(StringValue("--><--")),
      InterpolatedString(List("-->", MissingConstant, "<--")))
  }

  "ListValue" - {
    implicit val scope: Scope = NameToCheckedValueScope(MapView(
      "list" -> Right(ListValue(Seq[Value](
        -1, 111,
        ObjectValue(Map("elem" -> ListValue(Seq(StringValue("CONTENT"))))),
        ListValue(Seq(StringValue("DEEP"))))))))

    testEval("$list(0)",
      result = Right(-1),
      ArgumentExpr(NamedValue("list"), 0))

    testEval("${list}(0)",
      result = Right(-1),
      ArgumentExpr(NamedValue("list"), 0))

    testEval("$list(1)",
      result = Right(111),
      ArgumentExpr(NamedValue("list"), 1))

    testEval("$list(2).elem(0)",
      result = Right(StringValue("CONTENT")),
      ArgumentExpr(DotExpr(ArgumentExpr(NamedValue("list"), 2), "elem"), 0))

    testEval("$list(3)(0)",
      result = Right(StringValue("DEEP")),
      ArgumentExpr(ArgumentExpr(NamedValue("list"), 3), 0))

    testEval("$list(4)",
      result = Left(Problem.pure("Index 4 out of range 0...3")),
      ArgumentExpr(NamedValue("list"), 4))

    testEval("$list(-1)",
      result = Left(Problem.pure("Index -1 out of range 0...3")),
      ArgumentExpr(NamedValue("list"), -1))

    testEval("$list(1.5)",
      result = Left(Problem.pure("ArithmeticException: Rounding necessary")),
      ArgumentExpr(NamedValue("list"), BigDecimal("1.5")))
  }

  "&&" - {
    testEval("true && missing",
      result = Right(MissingValue),
      And(true, MissingConstant))

    testEval("missing && true",
      result = Right(MissingValue),
      And(MissingConstant, true))

    testEval("false && missing",
      result = Right(false),
      And(false, MissingConstant))

    testEval("missing && false",
      result = Right(MissingValue),
      And(MissingConstant, false))

    testEval("true && error('ERROR')",
      result = Left(ErrorInExpressionProblem("ERROR")),
      And(true, ErrorExpr("ERROR")))

    testEval("error('ERROR') && true",
      result = Left(ErrorInExpressionProblem("ERROR")),
      And(ErrorExpr("ERROR"), true))

    testEval("false && error('ERROR')",
      result = Right(false),
      And(false, ErrorExpr("ERROR")))

    testEval("error('ERROR') && false",
      result = Left(ErrorInExpressionProblem("ERROR")),
      And(ErrorExpr("ERROR"), false))

    "And" in {
      forAll((a: Boolean, b: Boolean) => assert(
        And(a, b).eval == Right(BooleanValue(a && b))))
    }
  }

  "||" - {
    testEval("true || missing",
      result = Right(true),
      Or(true, MissingConstant))

    testEval("missing || true",
      result = Right(MissingValue),
      Or(MissingConstant, true))

    testEval("false || missing",
      result = Right(MissingValue),
      Or(false, MissingConstant))

    testEval("missing || false",
      result = Right(MissingValue),
      Or(MissingConstant, false))

    testEval("true || error('ERROR')",
      result = Right(true),
      Or(true, ErrorExpr("ERROR")))

    testEval("error('ERROR') || true",
      result = Left(ErrorInExpressionProblem("ERROR")),
      Or(ErrorExpr("ERROR"), true))

    testEval("false || error('ERROR')",
      result = Left(ErrorInExpressionProblem("ERROR")),
      Or(false, ErrorExpr("ERROR")))

    testEval("error('ERROR') || false",
      result = Left(ErrorInExpressionProblem("ERROR")),
      Or(ErrorExpr("ERROR"), false))

    "Or" in {
      forAll((a: Boolean, b: Boolean) => assert(
        Or(a, b).eval == Right(BooleanValue(a || b))))
    }
  }

  "in" - {
    testEval("$returnCode in [1, 2, 3]",
      result = Right(true),
      In(LastReturnCode, ListExpr(List(1, 2, 3))))

    "In" in {
      forAll((a: Int, b: Int, c: Int, d: Int) => assert(
        In(a, ListExpr(List(b, c, d))).eval
          == Right(BooleanValue(Set(b, c, d)(a)))))
    }
  }

  "matches" - {
    testEval(""" "" matches "" """,
      result = Right(true),
      Matches("", ""))

    testEval(""" "" matches "A.+" """,
      result = Right(false),
      Matches("", "A.+"))

    testEval(""" "A" matches "A.+" """,
      result = Right(false),
      Matches("A", "A.+"))

    testEval(""" "-A-" matches "A.+" """,
      result = Right(false),
      Matches("-A-", "A.+"))

    testEval(""" "A--" matches "A.+" """,
      result = Right(true),
      Matches("A--", "A.+"))

    testEval(""" "A-" matches "A.+" """,
      result = Right(true),
      Matches("A-", "A.+"))

    testEval(""" variable("ASTRING") matches "A+" """,
      result = Right(true),
      Matches(NamedValue("ASTRING"), "A+"))

    testEval(""" 7 matches "" """,
      result = Right(false),
      Matches(7, ""))

    testEval(""" [7] matches "" """,
      result = Left(UnexpectedValueTypeProblem(StringValue, ListValue(List(7)))),
      Matches(ListExpr(List(7)), ""))

    testEval(""" missing matches "" """,
      result = Right(true),
      Matches(MissingConstant, ""))

    testEval(""" missing matches "A" """,
      result = Right(false),
      Matches(MissingConstant, "A"))

    testEval(""" "" matches missing """,
      result = Right(MissingValue),
      Matches("", MissingConstant))
  }

  "Complex expressions" - {
    testEval("""100 + 2 * 3 - 12 / 3""",
      result = Right(100 + 2 * 3 - 12 / 3),
      Substract(
        Add(100, Multiply(2, 3)),
        Divide(12, 3)))

    testEval("""$returnCode + 0 == 1""",
      result = Right(true),
      Equal(Add(LastReturnCode, 0), 1))

    testEval("""$returnCode + 1 == 2""",
      result = Right(true),
      Equal(Add(LastReturnCode, 1), 2))

    testEval("$returnCode >= 1 && !($returnCode <= 9) && $returnCode != 1",
      result = Right(false),
      And(
        And(
          GreaterOrEqual(LastReturnCode, 1),
          Not(LessOrEqual(LastReturnCode, 9))),
        NotEqual(LastReturnCode, 1)))

    "And and LessThan" in {
      assert(And(LessThan(1, 2), LessThan(1, ToNumber("7"))).eval ==
        Right(BooleanValue(true)))
    }
  }

  "Constant expressions" - {
    implicit val scope = Scope.empty
    val longString =
       """LINE 1
         |LINE 2
          LINE 3
         |"""

    testEval(
      s"'$longString'.stripMargin",
      result = Right(longString.stripMargin),
      StripMargin(longString))

    testEval("1 == 2",
      result = Right(false),
      Equal(1, 2))

    "Variables cannot be used" in {
      assert(NamedValue("VARIABLE").eval == Left(Problem("No such named value: VARIABLE")))
    }
  }

  private def testSyntaxError(exprString: String, problem: Problem)(implicit pos: source.Position): Unit =
    s"$exprString - should fail" in {
      assert(parseExpression(exprString) == Left(problem))
    }

  private def testEval(exprString: String, result: Checked[Value], expression: Expression)
    (implicit scope: Scope, pos: source.Position)
  : Unit =
    exprString in {
      val checked = parseExpressionOrFunction(exprString.trim)
      assert(checked == Right(expression))
      //if (checked != expression) {
      //  fail(diffx.Diff.compare(checked, expression).toString)
      //}
      assert(expression.eval == result)
    }
}
