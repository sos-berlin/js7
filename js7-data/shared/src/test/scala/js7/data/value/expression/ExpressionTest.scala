package js7.data.value.expression

import fastparse.NoWhitespace._
import js7.base.problem.{Checked, Problem}
import js7.data.job.JobResourcePath
import js7.data.parser.Parsers.checkedParse
import js7.data.value.ValueType.{MissingValueProblem, UnexpectedValueTypeProblem}
import js7.data.value.expression.Expression._
import js7.data.value.expression.ExpressionParser.expressionOnly
import js7.data.value.{BooleanValue, ListValue, NullValue, NumberValue, StringValue, Value}
import js7.data.workflow.Label
import js7.data.workflow.instructions.executable.WorkflowJob
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks._

final class ExpressionTest extends AnyFreeSpec
{
  "NamedValue expressions" - {
    implicit val scope =
      new Scope {
        import PositionSearch.{ByLabel, ByPrefix, ByWorkflowJob}
        import ValueSearch.{LastExecuted, Name}

        override def symbolToValue(symbol: String)(implicit scope: Scope) =
          symbol match {
            case "catchCount" => Some(Right(NumberValue(3)))
            case _ => None
          }

        override def findValue(search: ValueSearch)(implicit scope: Scope) =
          Right(search match {
            case ValueSearch(ValueSearch.LastOccurred, Name(name)) =>
              Map(
                "ASTRING" -> StringValue("AA"),
                "ANUMBER" -> NumberValue(7),
                "ABOOLEAN" -> BooleanValue(true),
                "returnCode" -> NumberValue(1)
              ).get(name)

            case ValueSearch(LastExecuted(ByPrefix("PREFIX")), Name(name)) =>
              Map("KEY" -> "LABEL-VALUE").get(name) map StringValue.apply

            case ValueSearch(LastExecuted(ByLabel(Label("LABEL"))), Name(name)) =>
              Map(
                "KEY" -> StringValue("LABEL-VALUE"),
                "returnCode" -> NumberValue(2)
              ).get(name)

            case ValueSearch(LastExecuted(ByWorkflowJob(WorkflowJob.Name("JOB"))), Name(name)) =>
              Map(
                "KEY" -> StringValue("JOB-VALUE"),
                "returnCode" -> NumberValue(3)
              ).get(name)

            case ValueSearch(ValueSearch.Argument, Name(name)) =>
              Map("ARG" -> "ARG-VALUE").get(name) map StringValue.apply

            case o =>
              None
          })

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

    testEval("{a: 'AAA'}.a",
      result = "AAA",
      Right(DotExpression(ObjectExpression(Map("a" -> StringConstant("AAA"))), "a")))

    testEval(""""-->$ASTRING${ABOOLEAN}$(100 + $ANUMBER)<--"""",
      result = "-->AAtrue107<--",
      Right(InterpolatedString(List(
        StringConstant("-->"),
        NamedValue("ASTRING"),
        NamedValue("ABOOLEAN"),
        Add(NumericConstant(100), NamedValue("ANUMBER")),
        StringConstant("<--")))))

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

    testEval("JobResource:myJobResource:VARIABLE",
      result = "myJobResource,VARIABLE,value",
      Right(JobResourceVariable(JobResourcePath("myJobResource"), Some("VARIABLE"))))

    testEval("JobResource:JOB-RESOURCE:`VARIABLE-NAME`",
      result = "JOB-RESOURCE,VARIABLE-NAME,value",
      Right(JobResourceVariable(JobResourcePath("JOB-RESOURCE"), Some("VARIABLE-NAME"))))

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

    testEval("""toNumber($ASTRING)""",
      result = Left(Problem("Not a valid number: AA")),
      Right(ToNumber(NamedValue.last("ASTRING"))))

    testEval("""toNumber('123')""",
      result = 123,
      Right(ToNumber(StringConstant("123"))))

    testEval("""$ASTRING.toNumber""",
      result = Left(Problem("Not a valid number: AA")),
      Right(ToNumber(NamedValue.last("ASTRING"))))

    testEval("""$ANUMBER""",
      result = 7,
      Right(NamedValue.last("ANUMBER")))

    testEval("""toBoolean('true')""",
      result = true,
      Right(ToBoolean(StringConstant("true"))))

    testEval("""toBoolean('false')""",
      result = false,
      Right(ToBoolean(StringConstant("false"))))

    testEval("""123.mkString""",
      result = "123",
      Right(MkString(NumericConstant(123))))

    testEval("stripMargin('  |ONE\n  |TWO')",
      result = "ONE\nTWO",
      Right(StripMargin(StringConstant("  |ONE\n  |TWO"))))

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

    testEval("""$returnCode == 1""",
      result = true,
      Right(Equal(LastReturnCode, NumericConstant(1))))

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

    testEval("""$returnCode + 0 == 1""",
      result = true,
      Right(Equal(Add(LastReturnCode, NumericConstant(0)), NumericConstant(1))))

    testEval("""$returnCode + 1 == 2""",
      result = true,
      Right(Equal(Add(LastReturnCode, NumericConstant(1)), NumericConstant(2))))

    testEval("""$returnCode - 3""",
      result = -2,
      Right(Substract(LastReturnCode, NumericConstant(3))))

    testEval("""'->' ++ $returnCode ++ '<-'""",
      result = "->1<-",
      Right(Concat(Concat(StringConstant("->"), LastReturnCode), StringConstant("<-"))))

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

    testEval(""" myFunction(7) """,
      result = 21,
      Right(FunctionCall("myFunction", Seq(Argument(NumericConstant(7))))))

    "objectExpression" in {
      val expr = Map(
        "A" -> NumericConstant(1),
        "B" -> StringConstant("BBB"),
        "LIST" -> ListExpression(List(NumericConstant(1), NumericConstant(2), NumericConstant(3))))
      assert(scope.evalExpressionMap(expr) ==
        Right(Map(
          "A" -> NumberValue(1),
          "B" -> StringValue("BBB"),
          "LIST" -> ListValue(List(NumberValue(1), NumberValue(2), NumberValue(3))))),
        Right(
          In(LastReturnCode, ListExpression(List(NumericConstant(1), NumericConstant(2), NumericConstant(3))))))
    }

    //testEval("""{"A": 1, "B": "BBB", "LIST": [1, 2, 3]}""",
    //  result = Right(ObjectValue(Map(
    //    "A" -> NumberValue(1),
    //    "B" -> StringValue("BBB"),
    //    "LIST" -> ListValue(List(NumberValue(1), NumberValue(2), NumberValue(3)))))),
    //  Right(
    //    In(LastReturnCode, ListExpression(List(NumericConstant(1), NumericConstant(2), NumericConstant(3))))))

    "Equal" in {
      forAll((a: Int, b: Int) => assert(
        Equal(NumericConstant(a), NumericConstant(b)).eval == Right(BooleanValue(a == b))))
      assert((Equal(NumericConstant(1), StringConstant("1"))).eval == Right(BooleanValue(false)))
    }

    "NotEqual" in {
      forAll((a: Int, b: Int) => assert(
        NotEqual(NumericConstant(a), NumericConstant(b)).eval == Right(BooleanValue(a != b))))
      assert((NotEqual(NumericConstant(1), StringConstant("1"))).eval == Right(BooleanValue(true)))
    }

    "LessOrEqual" in {
      forAll((a: Int, b: Int) => assert(
        LessOrEqual(NumericConstant(a), NumericConstant(b)).eval == Right(BooleanValue(a <= b))))
    }

    "GreaterOrEqual" in {
      forAll((a: Int, b: Int) => assert(
        GreaterOrEqual(NumericConstant(a), NumericConstant(b)).eval == Right(BooleanValue(a >= b))))
    }

    "LessThan" in {
      forAll((a: Int, b: Int) => assert(
        LessThan(NumericConstant(a), NumericConstant(b)).eval == Right(BooleanValue(a < b))))
    }

    "GreaterThan" in {
      forAll((a: Int, b: Int) => assert(
        GreaterThan(NumericConstant(a), NumericConstant(b)).eval == Right(BooleanValue(a > b))))
    }

    "In" in {
      forAll((a: Int, b: Int, c: Int, d: Int) => assert(
        In(NumericConstant(a), ListExpression(NumericConstant(b) :: NumericConstant(c) :: NumericConstant(d) :: Nil)).eval
          == Right(BooleanValue(Set(b, c, d)(a)))))
    }

    "Not" in {
      forAll((bool: Boolean) => assert(
        Not(BooleanConstant(bool)).eval == Right(BooleanValue(!bool))))
    }

    "And" in {
      forAll((a: Boolean, b: Boolean) => assert(
        And(BooleanConstant(a), BooleanConstant(b)).eval == Right(BooleanValue(a && b))))
    }

    "And is lazy" in {
      assert(And(BooleanConstant(true), booleanError).eval == Left(Problem("Not a valid number: X")))
      assert(And(BooleanConstant(false), booleanError).eval == Right(BooleanValue(false)))
    }

    "Or" in {
      forAll((a: Boolean, b: Boolean) => assert(
        Or(BooleanConstant(a), BooleanConstant(b)).eval == Right(BooleanValue(a || b))))
    }

    "Or is lazy" in {
      assert(Or(BooleanConstant(true), booleanError).eval == Right(BooleanValue(true)))
      assert(Or(BooleanConstant(false), booleanError).eval == Left(Problem("Not a valid number: X")))
    }

    "And and LessThan" in {
      assert(And(LessThan(NumericConstant(1), NumericConstant(2)), LessThan(NumericConstant(1), ToNumber(StringConstant("7")))).eval ==
        Right(BooleanValue(true)))
    }

    "mkString" in {
      assert(MkString(ListExpression(StringConstant("»") :: NamedValue.last("ASTRING") :: NumericConstant(7) :: Nil)).eval
        == Right(StringValue("»AA7")))
    }
  }

  "Missing value" - {
    implicit val scope = Scope.empty

    testEval("missing",
      result = Left(MissingValueProblem),
      Right(MissingConstant()))

    testEval("missing?",
      result = Right(NullValue),
      Right(OrNull(MissingConstant())))

    testEval("missing ?",
      result = Right(NullValue),
      Right(OrNull(MissingConstant())))

    testEval("missing orElse 7 + 3",
      result = Right(NumberValue(7 + 3)),
      Right(OrElse(
        MissingConstant(),
        Add(NumericConstant(7), NumericConstant(3)))))

    testEval("1 + 2 orElse 7 + 3",
      result = Right(NumberValue(1 + 2)),
      Right(OrElse(
        Add(NumericConstant(1), NumericConstant(2)),
        Add(NumericConstant(7), NumericConstant(3)))))

    testEval("1 + (2 orElse 7) + 3",
      result = Right(NumberValue(1 + 2 + 3)),
      Right(
        Add(
          Add(
            NumericConstant(1),
            OrElse(NumericConstant(2), NumericConstant(7))),
          NumericConstant(3))))

    testEval("missing == missing",
      result = Left(MissingValueProblem),
      Right(Equal(MissingConstant(), MissingConstant())))

    testEval("missing != missing",
      result = Left(MissingValueProblem),
      Right(NotEqual(MissingConstant(), MissingConstant())))

    testEval("missing + 1",
      result = Left(MissingValueProblem),
      Right(Add(MissingConstant(), NumericConstant(1))))

    "MissingValue with Problem" in {
      assert(MissingConstant(Problem("PROBLEM")).eval == Left(Problem("PROBLEM")))
    }

    "MissingValue with Problem, OrNull" in {
      assert(OrNull(MissingConstant(Problem("PROBLEM"))).eval == Right(NullValue))
      assert((OrNull(MissingConstant())).eval == Right(NullValue))
    }

    "MissingValue is not comparable" in {
      assert(Equal(MissingConstant(Problem("A")), MissingConstant(Problem("B"))).eval ==
        Left(Problem("A")))
    }

    testEval("\"-->$(missing)<--\"",
      result = Left(MissingValueProblem),
      Right(InterpolatedString(List(StringConstant("-->"), MissingConstant(), StringConstant("<--")))))

    "orElse" in {
      assert(OrElse(NumericConstant(1), NumericConstant(7)).eval == Right(NumberValue(1)))
      assert((OrElse(MissingConstant(), NumericConstant(7))).eval == Right(NumberValue(7)))
    }
  }

  "Null value" - {
    implicit val scope = Scope.empty

    testEval("null",
      result = Right(NullValue),
      Right(NullConstant))

    testEval("null?",
      result = Right(NullValue),
      Right(OrNull(NullConstant)))

    testEval("null ?",
      result = Right(NullValue),
      Right(OrNull(NullConstant)))

    testEval("null orElse 7 + 3",
      result = Right(NumberValue(7 + 3)),
      Right(OrElse(
        NullConstant,
        Add(NumericConstant(7), NumericConstant(3)))))

    testEval("null == null",
      result = Right(BooleanValue.True),
      Right(Equal(NullConstant, NullConstant)))

    testEval("null != null",
      result = Right(BooleanValue.False),
      Right(NotEqual(NullConstant, NullConstant)))

    testEval("null + 1",
      result = Left(UnexpectedValueTypeProblem(NumberValue, NullValue)),
      Right(Add(NullConstant, NumericConstant(1))))

    testEval("\"-->$(null)<--\"",
      result = Right(StringValue("--><--")),
      Right(InterpolatedString(List(StringConstant("-->"), NullConstant, StringConstant("<--")))))

    "orElse" in {
      assert((OrElse(NumericConstant(1), NumericConstant(7))).eval == Right(NumberValue(1)))
      assert((OrElse(NullConstant, NumericConstant(7))).eval == Right(NumberValue(7)))
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
      result = longString.stripMargin,
      Right(StripMargin(StringConstant(longString))))

    testEval("1 == 2",
      result = false,
      Right(Equal(NumericConstant(1), NumericConstant(2))))

    "Variables cannot be used" in {
      assert((NamedValue.last("VARIABLE")).eval == Left(Problem("No such named value: VARIABLE")))
    }
  }

  private def testSyntaxError(exprString: String, problem: String)(implicit pos: source.Position): Unit =
    registerTest(s"$exprString - should fail") {
      assert(ExpressionParser.parse(exprString) == Left(Problem(problem)))
    }

  private def testEval(exprString: String, result: Boolean, expression: Checked[Expression])(implicit scope: Scope, pos: source.Position): Unit =
    testEval(exprString, Right(BooleanValue(result)), expression)

  private def testEval(exprString: String, result: Int, expression: Checked[Expression])(implicit scope: Scope, pos: source.Position): Unit =
    testEval(exprString, Right(NumberValue(result)), expression)

  private def testEval(exprString: String, result: String, expression: Checked[Expression])(implicit scope: Scope, pos: source.Position): Unit =
    testEval(exprString, Right(StringValue(result)), expression)

  private def testEval(exprString: String, result: Checked[Value], expression: Checked[Expression])(implicit scope: Scope, pos: source.Position): Unit =
    registerTest(exprString) {
      val checked = checkedParse(exprString.trim, expressionOnly(_))
      assert(checked == expression)
      //if (checked != expression) {
      //  fail(diffx.Diff.compare(checked, expression).toString)
      //}
      for (e <- expression) {
        assert(checkedParse(e.toString, expressionOnly(_)) == expression, " in toString❗")
        assert(e.eval == result)
      }
    }
}