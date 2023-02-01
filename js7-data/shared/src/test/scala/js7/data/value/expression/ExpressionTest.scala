package js7.data.value.expression

import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.data.job.JobResourcePath
import js7.data.value.ValueType.{MissingValueProblem, UnexpectedValueTypeProblem}
import js7.data.value.expression.Expression.*
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
    val booleanError: BooleanExpression = LessThan(ToNumber(StringConstant("X")), NumericConstant(7))

    testEval("7",
      result = 7,
      Right(NumericConstant(7)))

    testEval(Long.MinValue.toString,
      result = Long.MinValue,
      Right(NumericConstant(Long.MinValue)))

    testEval("-1.111222333444555666777888999",
      result = BigDecimal("-1.111222333444555666777888999"),
      Right(NumericConstant(BigDecimal("-1.111222333444555666777888999"))))

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

    locally {
      val number = "-111222333444555666777888999000111222333444555666777888999000"
      testEval(number,
        result = Right(NumberValue(BigDecimal(number))),
        Right(NumericConstant(BigDecimal(number))))
    }

    testEval("$ASTRING",
      result = "AA",
      Right(NamedValue("ASTRING")))

    testEval("${ASTRING}",
      result = "AA",
      Right(NamedValue("ASTRING")))

    testEval("JobResource:myJobResource:VARIABLE",
      result = "myJobResource,VARIABLE,value",
      Right(JobResourceVariable(JobResourcePath("myJobResource"), Some("VARIABLE"))))

    testEval("JobResource:JOB-RESOURCE:`VARIABLE-NAME`",
      result = "JOB-RESOURCE,VARIABLE-NAME,value",
      Right(JobResourceVariable(JobResourcePath("JOB-RESOURCE"), Some("VARIABLE-NAME"))))

    //testEval("${label::LABEL.KEY}",
    //  result = "LABEL-VALUE",
    //  Right(NamedValue(NamedValue.ByLabel(Label("LABEL")), StringConstant("KEY"))))
    //
    //testEval("${job::JOB.KEY}",
    //  result = "JOB-VALUE",
    //  Right(NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), StringConstant("KEY"))))

    testEval("$UNKNOWN",
      result = Left(Problem("No such named value: UNKNOWN")),
      Right(NamedValue("UNKNOWN")))

    testEval("""variable("ASTRING")""",
      result = "AA",
      Right(NamedValue("ASTRING")))

    testEval("""variable(key="ASTRING")""",
      result = "AA",
      Right(NamedValue("ASTRING")))

    testEval("""variable("UNKNOWN")""",
      result = Left(Problem("No such named value: UNKNOWN")),
      Right(NamedValue("UNKNOWN")))

    testEval("""variable("UNKNOWN", default="DEFAULT")""",
      result = "DEFAULT",
      Right(NamedValue("UNKNOWN", StringConstant("DEFAULT"))))

    testEval("""variable(job=JOB, key="UNKNOWN", default="DEFAULT")""",
      result = "DEFAULT",
      Right(NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), StringConstant("UNKNOWN"), Some(StringConstant("DEFAULT")))))

    testEval("""variable(label=LABEL, key="UNKNOWN", default="DEFAULT")""",
      result = "DEFAULT",
      Right(NamedValue(NamedValue.ByLabel("LABEL"), StringConstant("UNKNOWN"), Some(StringConstant("DEFAULT")))))

    testEval("""argument("ARG")""",
      result = "ARG-VALUE",
      Right(NamedValue(NamedValue.Argument, StringConstant("ARG"))))

    testEval("""argument(key="ARG")""",
      result = "ARG-VALUE",
      Right(NamedValue(NamedValue.Argument, StringConstant("ARG"))))

    testEval("""argument("UNKNOWN", default="DEFAULT")""",
      result = "DEFAULT",
      Right(NamedValue(NamedValue.Argument, StringConstant("UNKNOWN"), Some(StringConstant("DEFAULT")))))

    testEval("""variable(key="returnCode", label=LABEL)""",
      result = 2,
      Right(NamedValue(NamedValue.ByLabel("LABEL"), StringConstant("returnCode"))))

    testEval("""variable(key="returnCode", job=JOB)""",
      result = 3,
      Right(NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), StringConstant("returnCode"))))

    testEval("""toNumber($ASTRING)""",
      result = Left(Problem("Not a valid number: AA")),
      Right(ToNumber(NamedValue("ASTRING"))))

    testEval("""toNumber('123')""",
      result = 123,
      Right(ToNumber(StringConstant("123"))))

    testEval("""$ASTRING.toNumber""",
      result = Left(Problem("Not a valid number: AA")),
      Right(ToNumber(NamedValue("ASTRING"))))

    testEval("""$ANUMBER""",
      result = 7,
      Right(NamedValue("ANUMBER")))

    testEval("""$myObject""",
      result = Right(ObjectValue(Map("myField" -> ObjectValue(Map("a" -> NumberValue(1)))))),
      Right(NamedValue("myObject")))

    testEval("""$myObject.myField.a""",
      result = 1,
      Right(DotExpression(DotExpression(NamedValue("myObject"), "myField"), "a")))

    testEval(""""$($myObject.myField.a)"""",
      result = "1",
      Right(InterpolatedString(List(
        DotExpression(DotExpression(NamedValue("myObject"), "myField"), "a")))))

    testEval(""""$ANUMBER-$($myObject.myField.a)"""",
      result = "7-1",
      Right(InterpolatedString(List(
        NamedValue("ANUMBER"),
        StringConstant("-"),
        DotExpression(DotExpression(NamedValue("myObject"), "myField"), "a")))))

    testEval(""""${myObject.myField.a}"""",
      result = "1",
      Right(InterpolatedString(List(
        DotExpression(DotExpression(NamedValue("myObject"), "myField"), "a")))))

    testEval("""toBoolean('true')""",
      result = true,
      Right(ToBoolean(StringConstant("true"))))

    testEval("""toBoolean('false')""",
      result = false,
      Right(ToBoolean(StringConstant("false"))))

    testEval("""mkString(123)""",
      result = "123",
      Right(MkString(NumericConstant(123))))

    testEval("""mkString([$ANUMBER, "-", true])""",
      result = "7-true",
      Right(MkString(ListExpression(List(
        NamedValue("ANUMBER"),
        StringConstant("-"),
        BooleanConstant(true))))))

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
      Right(NamedValue("ABOOLEAN")))

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

    testEval("""$returnCode * 3 == 3""",
      result = true,
      Right(Equal(Multiply(LastReturnCode, NumericConstant(3)), NumericConstant(3))))

    testEval("""1 / 3""",
      result = Right(NumberValue(BigDecimal("0.3333333333333333333333333333333333"))),
      Right(Divide(NumericConstant(1), NumericConstant(3))))

    testEval("""(6 / 3)?""",
      result = Right(NumberValue(2)),
      Right(OrNull(Divide(NumericConstant(6), NumericConstant(3)))))

    testEval("""1 / 0""",
      result = Left(Problem("java.lang.ArithmeticException: Division by zero")),
      Right(Divide(NumericConstant(1), NumericConstant(0))))

    testEval("""(1 / 0)?""",
      result = Right(NullValue),
      Right(OrNull(Divide(NumericConstant(1), NumericConstant(0)))))

    testEval("""(1 / 0) orElse -1""",
      result = Right(NumberValue(-1)),
      Right(OrElse(Divide(NumericConstant(1), NumericConstant(0)), NumericConstant(-1))))

    testEval("""100 + 2 * 3 - 12 / 3""",
      result = Right(NumberValue(100 + 2 * 3 - 12 / 3)),
      Right(
        Substract(
          Add(
            NumericConstant(100),
            Multiply(NumericConstant(2), NumericConstant(3))),
          Divide(NumericConstant(12), NumericConstant(3)))))

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
      Right(Matches(NamedValue("ASTRING"), StringConstant("A+"))))

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

    "replaceAll" in {
      assert(ReplaceAll(StringConstant("abcdef"), StringConstant("([ae])"), StringConstant("($1)")).eval
        == Right(StringValue("(a)bcd(e)f")))
    }

    "mkString" in {
      assert(MkString(ListExpression(StringConstant("»") :: NamedValue("ASTRING") :: NumericConstant(7) :: Nil)).eval
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
      assert(OrElse(NamedValue("UNKNOWN"), NumericConstant(1)).eval == Right(NumberValue(1)))
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

  "ListValue" - {
    implicit val scope: Scope = NameToCheckedValueScope(MapView(
      "list" -> Right(ListValue(Seq(NumberValue(-1), NumberValue(111), NumberValue(222))))))

    testEval("$list(0)",
      result = Right(NumberValue(-1)),
      Right(ArgumentExpression(NamedValue("list"), NumericConstant(0))))

    testEval("${list}(0)",
      result = Right(NumberValue(-1)),
      Right(ArgumentExpression(NamedValue("list"), NumericConstant(0))))

    testEval("$list(1)",
      result = Right(NumberValue(111)),
      Right(ArgumentExpression(NamedValue("list"), NumericConstant(1))))

    testEval("$list(2)",
      result = Right(NumberValue(222)),
      Right(ArgumentExpression(NamedValue("list"), NumericConstant(2))))

    testEval("$list(3)",
      result = Left(Problem.pure("Index 3 out of range 0...2")),
      Right(ArgumentExpression(NamedValue("list"), NumericConstant(3))))

    testEval("$list(-1)",
      result = Left(Problem.pure("Index -1 out of range 0...2")),
      Right(ArgumentExpression(NamedValue("list"), NumericConstant(-1))))

    testEval("$list(1.5)",
      result = Left(Problem.pure("java.lang.ArithmeticException: Rounding necessary")),
      Right(ArgumentExpression(NamedValue("list"), NumericConstant(BigDecimal("1.5")))))
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
      assert((NamedValue("VARIABLE")).eval == Left(Problem("No such named value: VARIABLE")))
    }
  }

  private def testSyntaxError(exprString: String, problem: String)(implicit pos: source.Position): Unit =
    s"$exprString - should fail" in {
      assert(parseExpression(exprString) == Left(Problem(problem)))
    }

  private def testEval(exprString: String, result: Boolean, expression: Checked[Expression])
    (implicit scope: Scope, pos: source.Position)
  : Unit =
    testEval(exprString, Right(BooleanValue(result)), expression)

  private def testEval(exprString: String, result: BigDecimal, expression: Checked[Expression])
    (implicit scope: Scope, pos: source.Position)
  : Unit =
    testEval(exprString, Right(NumberValue(result)), expression)

  private def testEval(exprString: String, result: String, expression: Checked[Expression])
    (implicit scope: Scope, pos: source.Position)
  : Unit =
    testEval(exprString, Right(StringValue(result)), expression)

  private def testEval(exprString: String, result: Checked[Value], expression: Checked[Expression])
    (implicit scope: Scope, pos: source.Position)
  : Unit =
    exprString in {
      val checked = parseExpressionOrFunction(exprString.trim)
      assert(checked == expression)
      //if (checked != expression) {
      //  fail(diffx.Diff.compare(checked, expression).toString)
      //}
      for (e <- expression) {
        assert(parseExpression(e.toString) == expression, s" in -->toString=${e.toString}")
        assert(e.eval == result)
      }
    }
}
