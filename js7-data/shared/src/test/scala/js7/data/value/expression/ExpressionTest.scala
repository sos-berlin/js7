package js7.data.value.expression

import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.data.job.JobResourcePath
import js7.data.value.Value.convenience.*
import js7.data.value.ValueType.{MissingValueProblem, UnexpectedValueTypeProblem}
import js7.data.value.expression.Expression.*
import js7.data.value.expression.Expression.convenience.*
import js7.data.value.expression.ExpressionParser.{parseExpression, parseExpressionOrFunction}
import js7.data.value.expression.scopes.NameToCheckedValueScope
import js7.data.value.{BooleanValue, ListValue, NullValue, NumberValue, ObjectValue, StringValue, Value}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Label
import org.scalactic.source
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks.*
import scala.collection.MapView

final class ExpressionTest extends OurTestSuite
{
  "NamedValue expressions" - {
    implicit val scope: Scope =
      new Scope {
        import PositionSearch.{ByLabel, ByPrefix, ByWorkflowJob}
        import ValueSearch.{LastExecuted, Name}

        override def symbolToValue(symbol: String) =
          symbol match {
            case "catchCount" => Some(Right(NumberValue(3)))
            case _ => None
          }

        override lazy val nameToCheckedValue =
          MapView(
            "ASTRING" -> Right(StringValue("AA")),
            "ANUMBER" -> Right(NumberValue(7)),
            "ABOOLEAN" -> Right(BooleanValue(true)),
            "returnCode" -> Right(NumberValue(1)),
            "myObject" -> Right(ObjectValue(Map(
                "myField" -> ObjectValue(Map(
                  "a" -> NumberValue(1)))))))

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

            case o =>
              None
          }

        override def evalFunctionCall(functionCall: FunctionCall)(implicit scope: Scope) =
          functionCall match {
            case FunctionCall("myFunction", Seq(Argument(expr, None))) =>
             Some(expr.eval.flatMap(_.asNumber).map(n => NumberValue(n * 3)))

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
    val booleanError: BooleanExpression = LessThan(ToNumber("X"), 7)

    testEval("7",
      result = Right(7),
      7)

    testEval(Long.MinValue.toString,
      result = Right(Long.MinValue),
      Long.MinValue)

    testEval("-1.111222333444555666777888999",
      result = Right(BigDecimal("-1.111222333444555666777888999")),
      BigDecimal("-1.111222333444555666777888999"))

    testEval(""" "" """,
      result = Right(""),
      "")

    testEval( """ "\\" """,
      result = Right("\\"),
      "\\")

    testEval("{a: 'AAA'}.a",
      result = Right("AAA"),
      DotExpression(ObjectExpression(Map("a" -> "AAA")), "a"))

    testEval(""""-->$ASTRING${ABOOLEAN}$(100 + $ANUMBER)<--"""",
      result = Right("-->AAtrue107<--"),
      InterpolatedString(List(
        "-->",
        NamedValue("ASTRING"),
        NamedValue("ABOOLEAN"),
        Add(100, NamedValue("ANUMBER")),
        "<--")))

    testEval(""" "x" """,
      result = Right("x"),
      "x")

    testEval(""" 'a\x' """,
      result = Right("a\\x"),
      "a\\x")

    testEval("false",
      result = Right(false),
      false)

    testEval("true",
      result = Right(true),
      true)

    locally {
      val number = "-111222333444555666777888999000111222333444555666777888999000"
      testEval(number,
        result = Right(NumberValue(BigDecimal(number))),
        BigDecimal(number))
    }

    testEval("$ASTRING",
      result = Right("AA"),
      NamedValue("ASTRING"))

    testEval("${ASTRING}",
      result = Right("AA"),
      NamedValue("ASTRING"))

    testEval("JobResource:myJobResource:VARIABLE",
      result = Right("myJobResource,VARIABLE,value"),
      JobResourceVariable(JobResourcePath("myJobResource"), Some("VARIABLE")))

    testEval("JobResource:JOB-RESOURCE:`VARIABLE-NAME`",
      result = Right("JOB-RESOURCE,VARIABLE-NAME,value"),
      JobResourceVariable(JobResourcePath("JOB-RESOURCE"), Some("VARIABLE-NAME")))

    //testEval("${label::LABEL.KEY}",
    //  result = Right("LABEL-VALUE"),
    //  Right(NamedValue(NamedValue.ByLabel(Label("LABEL")), ("KEY"))))
    //
    //testEval("${job::JOB.KEY}",
    //  result = Right("JOB-VALUE"),
    //  Right(NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), ("KEY"))))

    testEval("$UNKNOWN",
      result = Left(Problem("No such named value: UNKNOWN")),
      NamedValue("UNKNOWN"))

    testEval("""variable("ASTRING")""",
      result = Right("AA"),
      NamedValue("ASTRING"))

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

    testEval("""argument("ARG")""",
      result = Right("ARG-VALUE"),
      NamedValue(NamedValue.Argument, "ARG"))

    testEval("""argument(key="ARG")""",
      result = Right("ARG-VALUE"),
      NamedValue(NamedValue.Argument, "ARG"))

    testEval("""argument("UNKNOWN", default="DEFAULT")""",
      result = Right("DEFAULT"),
      NamedValue(NamedValue.Argument, "UNKNOWN", Some("DEFAULT")))

    testEval("""variable(key="returnCode", label=LABEL)""",
      result = Right(2),
      NamedValue(NamedValue.ByLabel("LABEL"), "returnCode"))

    testEval("""variable(key="returnCode", job=JOB)""",
      result = Right(3),
      NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), "returnCode"))

    testEval("""toNumber($ASTRING)""",
      result = Left(Problem("Not a valid number: AA")),
      ToNumber(NamedValue("ASTRING")))

    testEval("""toNumber('123')""",
      result = Right(123),
      ToNumber("123"))

    testEval("""$ASTRING.toNumber""",
      result = Left(Problem("Not a valid number: AA")),
      ToNumber(NamedValue("ASTRING")))

    testEval("""$ANUMBER""",
      result = Right(7),
      NamedValue("ANUMBER"))

    testEval("""$myObject""",
      result = Right(ObjectValue(Map("myField" -> ObjectValue(Map("a" -> NumberValue(1)))))),
      NamedValue("myObject"))

    testEval("""3 + $myObject.myField.a""",
      result = Right(4),
      Add(
        3,
        DotExpression(DotExpression(NamedValue("myObject"), "myField"), "a")))

    testEval(""""$($myObject.myField.a)"""",
      result = Right("1"),
      InterpolatedString(List(
        DotExpression(DotExpression(NamedValue("myObject"), "myField"), "a"))))

    testEval(""""$ANUMBER-$($myObject.myField.a)"""",
      result = Right("7-1"),
      InterpolatedString(List(
        NamedValue("ANUMBER"),
        "-",
        DotExpression(DotExpression(NamedValue("myObject"), "myField"), "a"))))

    testEval(""""${myObject.myField.a}"""",
      result = Right("1"),
      InterpolatedString(List(
        DotExpression(DotExpression(NamedValue("myObject"), "myField"), "a"))))

    testEval("""toBoolean('true')""",
      result = Right(true),
      ToBoolean("true"))

    testEval("""toBoolean('false')""",
      result = Right(false),
      ToBoolean("false"))

    testEval("""mkString(123)""",
      result = Right("123"),
      MkString(123))

    testEval("""mkString([$ANUMBER, "-", true])""",
      result = Right("7-true"),
      MkString(ListExpression(List(
        NamedValue("ANUMBER"),
        "-",
        true))))

    testEval("stripMargin('  |ONE\n  |TWO')",
      result = Right("ONE\nTWO"),
      StripMargin("  |ONE\n  |TWO"))

    testEval(""""true".toBoolean""",
      result = Right(true),
      ToBoolean("true"))

    testEval(""""false".toBoolean""",
      result = Right(false),
      ToBoolean("false"))

    testEval(""" variable("ABOOLEAN")""",
      result = Right(true),
      NamedValue("ABOOLEAN"))

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

    testEval("""$returnCode == 1""",
      result = Right(true),
      Equal(LastReturnCode, 1))

    testEval("""$returnCode == 0""",
      result = Right(false),
      Equal(LastReturnCode, 0))

    testEval("""$returnCode >= 1""",
      result = Right(true),
      GreaterOrEqual(LastReturnCode, 1))

    testEval("""$returnCode <= 1""",
      result = Right(true),
      LessOrEqual(LastReturnCode, 1))

    testEval("""$returnCode > 1""",
      result = Right(false),
      GreaterThan(LastReturnCode, 1))

    testEval("""$returnCode < 1""",
      result = Right(false),
      LessThan(LastReturnCode, 1))

    testEval("""$returnCode * 3 == 3""",
      result = Right(true),
      Equal(Multiply(LastReturnCode, 3), 3))

    testEval("""1 / 3""",
      result = Right(NumberValue(BigDecimal("0.3333333333333333333333333333333333"))),
      Divide(1, 3))

    testEval("""1 / 0""",
      result = Left(Problem("java.lang.ArithmeticException: Division by zero")),
      Divide(1, 0))

    testEval("""100 + 2 * 3 - 12 / 3""",
      result = Right(NumberValue(100 + 2 * 3 - 12 / 3)),
      Substract(
        Add(100, Multiply(2, 3)),
        Divide(12, 3)))

    testEval("""$returnCode + 0 == 1""",
      result = Right(true),
      Equal(Add(LastReturnCode, 0), 1))

    testEval("""$returnCode + 1 == 2""",
      result = Right(true),
      Equal(Add(LastReturnCode, 1), 2))

    testEval("""$returnCode - 3""",
      result = Right(-2),
      Substract(LastReturnCode, 3))

    testEval("""'->' ++ $returnCode ++ '<-'""",
      result = Right("->1<-"),
      Concat(Concat("->", LastReturnCode), "<-"))

    testEval("""catchCount""",
      result = Right(3),
      OrderCatchCount)

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

    testEval("!false",
      result = Right(true),
      Not(false))

    testEval("! true",
      result = Right(false),
      Not(true))

    testEval("!!true",
      result = Right(true),
      Not(Not(true)))

    testEval("$returnCode >= 1 && !($returnCode <= 9) && $returnCode != 1",
      result = Right(false),
      And(
        And(
          GreaterOrEqual(LastReturnCode, 1),
          Not(LessOrEqual(LastReturnCode, 9))),
        NotEqual(LastReturnCode, 1)))

    testEval("$returnCode in [1, 2, 3]",
      result = Right(true),
      In(LastReturnCode, ListExpression(List(1, 2, 3))))

    testEval(""" myFunction(7) """,
      result = Right(21),
      FunctionCall("myFunction", Seq(Argument(7))))

    "objectExpression" in {
      val expr = Map[String, Expression](
        "A" -> 1,
        "B" -> "BBB",
        "LIST" -> ListExpression(List(1, 2, 3)))
      assert(scope.evalExpressionMap(expr) ==
        Right(Map[String, Value](
          "A" -> 1,
          "B" -> "BBB",
          "LIST" -> ListValue(List[Value](1, 2, 3)))),
        In(LastReturnCode, ListExpression(List(1, 2, 3))))
    }

    //testEval("""{"A": 1, "B": "BBB", "LIST": [1, 2, 3]}""",
    //  result = Right(ObjectValue(Map(
    //    "A" -> NumberValue(1),
    //    "B" -> StringValue("BBB"),
    //    "LIST" -> ListValue(List(NumberValue(1), NumberValue(2), NumberValue(3)))))),
    //  In(LastReturnCode, ListExpression(List((1), (2), (3)))))

    "Equal" in {
      forAll((a: Int, b: Int) => assert(
        Equal(a, b).eval == Right(BooleanValue(a == b))))
      assert(Equal(1, "1").eval == Right(BooleanValue(false)))
    }

    "NotEqual" in {
      forAll((a: Int, b: Int) => assert(
        NotEqual(a, b).eval == Right(BooleanValue(a != b))))
      assert(NotEqual(1, "1").eval == Right(BooleanValue(true)))
    }

    "LessOrEqual" in {
      forAll((a: Int, b: Int) => assert(
        LessOrEqual(a, b).eval == Right(BooleanValue(a <= b))))
    }

    "GreaterOrEqual" in {
      forAll((a: Int, b: Int) => assert(
        GreaterOrEqual(a, b).eval == Right(BooleanValue(a >= b))))
    }

    "LessThan" in {
      forAll((a: Int, b: Int) => assert(
        LessThan(a, b).eval == Right(BooleanValue(a < b))))
    }

    "GreaterThan" in {
      forAll((a: Int, b: Int) => assert(
        GreaterThan(a, b).eval == Right(BooleanValue(a > b))))
    }

    "In" in {
      forAll((a: Int, b: Int, c: Int, d: Int) => assert(
        In(a, ListExpression(List(b, c, d))).eval
          == Right(BooleanValue(Set(b, c, d)(a)))))
    }

    "OrNull, OrElse" - {
      testEval("missing ?",
        result = Right(NullValue),
        OrNull(MissingConstant))

      testEval("missing ? 7 + 3",
        result = Right(NumberValue(7 + 3)),
        Add(
          OrElse(MissingConstant, 7),
          3))

      testEval("$unknown ? 7 + 3",
        result = Right(NumberValue(7 + 3)),
        Add(
          OrElse(NamedValue("unknown"), 7),
          3))

      testEval("1 + 2 ? 7 + 3",
        result = Right(NumberValue(1 + 2 + 3)),
        Add(
          Add(
            1,
            OrElse(2, 7)),
          3))

      testEval("1 + (2 ? 7) + 3",
        result = Right(NumberValue(1 + 2 + 3)),
        Add(
          Add(1, OrElse(2, 7)),
          3))

      testEval("""missing? ?""",
        result = Right(NullValue),
        OrNull(OrNull(MissingConstant)))

      testEval("""missing? ? 7""",
        result = Right(NumberValue(7)),
        OrElse(OrNull(MissingConstant), 7))

      // Reserve ?? for future use
      testSyntaxError("""missing ??""", Problem(
        "Error in expression: Parsing failed at position 10 “missing ?❓?” · Unexpected “?”"))

      testEval("""(missing?)?""",
        result = Right(NullValue),
        OrNull(OrNull(MissingConstant)))

      testEval("null?",
        result = Right(NullValue),
        OrNull(NullConstant))

      testEval("null ? 7 + 3",
        result = Right(NumberValue(7 + 3)),
        Add(
          OrElse(NullConstant, 7),
          3))

      testEval("""6 / 3?""",
        result = Right(NumberValue(2)),
        Divide(6, OrNull(3)))

      testEval("""(1 / 0)?""",
        result = Right(NullValue),
        OrNull(Divide(1, 0)))

      testEval("""(1 / 0) ? -1""",
        result = Right(NumberValue(-1)),
        OrElse(Divide(1, 0), -1))

      testEval("""(7 in [ 1 / 0 ]) ? $unknown ? -1""",
        result = Right(NumberValue(-1)),
        OrElse(
          OrElse(
            In(
              7,
              ListExpression(List(Divide(1, 0)))),
            NamedValue("unknown")),
          -1))

      testEval("""$aUnknown ? $bUnknown ? """,
        result = Right(NullValue),
        OrNull(
          OrElse(
            NamedValue("aUnknown"),
            NamedValue("bUnknown"))))
    }


    "Not" in {
      forAll((bool: Boolean) => assert(
        Not(bool).eval == Right(BooleanValue(!bool))))
    }

    "And" in {
      forAll((a: Boolean, b: Boolean) => assert(
        And(a, b).eval == Right(BooleanValue(a && b))))
    }

    testEval("""true || false && 3 == 4 < 5 + 6 * 7 ? 8""",
      result = Right(BooleanValue(true)),
      Or(true, And(false, Equal(3, LessThan(4, Add(5, Multiply(6, OrElse(7, 8))))))))


    "And is lazy" in {
      assert(And(true, booleanError).eval == Left(Problem("Not a valid number: X")))
      assert(And(false, booleanError).eval == Right(BooleanValue(false)))
    }

    "Or" in {
      forAll((a: Boolean, b: Boolean) => assert(
        Or(a, b).eval == Right(BooleanValue(a || b))))
    }

    "Or is lazy" in {
      assert(Or(true, booleanError).eval == Right(BooleanValue(true)))
      assert(Or(false, booleanError).eval == Left(Problem("Not a valid number: X")))
    }

    "And and LessThan" in {
      assert(And(LessThan(1, 2), LessThan(1, ToNumber("7"))).eval ==
        Right(BooleanValue(true)))
    }

    "replaceAll" in {
      assert(ReplaceAll("abcdef", "([ae])", "($1)").eval
        == Right(StringValue("(a)bcd(e)f")))
    }

    "mkString" in {
      assert(MkString(ListExpression(List("»", NamedValue("ASTRING"), 7))).eval
        == Right(StringValue("»AA7")))
    }
  }

  "Missing value" - {
    implicit val scope = Scope.empty

    testEval("missing",
      result = Left(MissingValueProblem("missing")),
      MissingConstant)

    testEval("missing?",
      result = Right(NullValue),
      OrNull(MissingConstant))

    testEval("missing ?",
      result = Right(NullValue),
      OrNull(MissingConstant))

    testEval("missing == missing",
      result = Left(MissingValueProblem("missing")),
      Equal(MissingConstant, MissingConstant))

    testEval("missing != missing",
      result = Left(MissingValueProblem("missing")),
      NotEqual(MissingConstant, MissingConstant))

    testEval("missing + 1",
      result = Left(MissingValueProblem("missing")),
      Add(MissingConstant, 1))

    "MissingValue OrNull" in {
      assert(OrNull(MissingConstant).eval == Right(NullValue))
    }

    "MissingValue is not comparable" in {
      assert(Equal(MissingConstant, MissingConstant).eval == Left(MissingValueProblem("missing")))
    }

    testEval("\"-->$(missing)<--\"",
      result = Left(MissingValueProblem("missing")),
      InterpolatedString(List("-->", MissingConstant, "<--")))

    testEval("\"-->$(missing?)<--\"",
      result = Right(StringValue("--><--")),
      InterpolatedString(List("-->", OrNull(MissingConstant), "<--")))
  }

  "Null value" - {
    implicit val scope = Scope.empty

    testEval("null",
      result = Right(NullValue),
      NullConstant)

    testEval("null == null",
      result = Right(BooleanValue.True),
      Equal(NullConstant, NullConstant))

    testEval("null != null",
      result = Right(BooleanValue.False),
      NotEqual(NullConstant, NullConstant))

    testEval("null + 1",
      result = Left(UnexpectedValueTypeProblem(NumberValue, NullValue)),
      Add(NullConstant, 1))

    testEval("\"-->$(null)<--\"",
      result = Right(StringValue("--><--")),
      InterpolatedString(List("-->", NullConstant, "<--")))
  }

  "ListValue" - {
    implicit val scope: Scope = NameToCheckedValueScope(MapView(
      "list" -> Right(ListValue(Seq[Value](-1, 111, 222)))))

    testEval("$list(0)",
      result = Right(NumberValue(-1)),
      ArgumentExpression(NamedValue("list"), 0))

    testEval("${list}(0)",
      result = Right(NumberValue(-1)),
      ArgumentExpression(NamedValue("list"), 0))

    testEval("$list(1)",
      result = Right(NumberValue(111)),
      ArgumentExpression(NamedValue("list"), 1))

    testEval("$list(2)",
      result = Right(NumberValue(222)),
      ArgumentExpression(NamedValue("list"), 2))

    testEval("$list(3)",
      result = Left(Problem.pure("Index 3 out of range 0...2")),
      ArgumentExpression(NamedValue("list"), 3))

    testEval("$list(-1)",
      result = Left(Problem.pure("Index -1 out of range 0...2")),
      ArgumentExpression(NamedValue("list"), -1))

    testEval("$list(1.5)",
      result = Left(Problem.pure("java.lang.ArithmeticException: Rounding necessary")),
      ArgumentExpression(NamedValue("list"), BigDecimal("1.5")))
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
