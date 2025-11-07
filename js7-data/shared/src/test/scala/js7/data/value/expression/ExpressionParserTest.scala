package js7.data.value.expression

import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.data.value.expression.Expression.*
import js7.data.value.expression.Expression.BooleanConstant.True
import js7.data.value.expression.Expression.convenience.given
import js7.data.value.expression.ExpressionParser.*
import js7.data.workflow.instructions.executable.WorkflowJob
import org.scalactic.source
import scala.language.implicitConversions

final class ExpressionParserTest extends OurTestSuite:
  // See also ExpressionTest

  "NamedValue" - {
    "$ with impossible names" in:
      assert(dollarNamedValue.parse("$var/1") == Right("/1" -> NamedValue("var")))
      assert(dollarNamedValue.parse("$var.1") == Right(".1" -> NamedValue("var")))
      assert(dollarNamedValue.parse("$var-1") == Right("-1" -> NamedValue("var")))
      assert(dollarNamedValue.parse("$var_1") == Right("" -> NamedValue("var_1")))

    testExpression("""$key""", NamedValue("key"))
    testExpression("""$under_line""", NamedValue("under_line"))
    testExpression("""$Schlüssel""", NamedValue("Schlüssel"))
    testExpression("""$clé""", NamedValue("clé"))
    testExpression("""$A""", NamedValue("A"))
    testExpression("""${SOME_KEY}""", NamedValue("SOME_KEY"))
    testExpression("""$`weird name, with dot., and comma`""", NamedValue("weird name, with dot., and comma"))
    testExpression("""${`weird name, with dot., and comma`}""", NamedValue("weird name, with dot., and comma"))
    testExpression(""""${`weird name, with dot., and comma`}"""", InterpolatedString(List(NamedValue("weird name, with dot., and comma"))))
    //testExpression("""${arg::SOME-KEY}""", NamedValue(NamedValue.Argument, "SOME-KEY"))
    //testExpression("""${label::LABEL.SOME-KEY}""", NamedValue(NamedValue.ByLabel(Label("LABEL")), "SOME-KEY"))
    //testExpression("""${job::JOB.SOME-KEY}""", NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), "SOME-KEY"))
    //testExpression("""${A.SOME-KEY}""", NamedValue(NamedValue.LastOccurredByPrefix("A"), "SOME-KEY"))

    "variable()" in:
      assert(parseExpression("""variable("clé")""") ==
        Right(NamedValue(NamedValue.LastOccurred, StringConstant("clé"))))
      assert(parseExpression("""variable ( "clé", default = "DEFAULT" )""") ==
        Right(NamedValue("clé", "DEFAULT")))
      assert(parseExpression("""variable(key="clé", label=LABEL)""") ==
        Right(NamedValue(NamedValue.ByLabel("LABEL"), "clé")))
      assert(parseExpression("""variable(key="clé", job=JOB)""") ==
        Right(NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), "clé")))

    "argument()" in:
      assert(parseExpression("""argument("clé")""") ==
        Right(NamedValue(NamedValue.Argument, "clé")))
      assert(parseExpression("""argument ( "clé", default = "DEFAULT" )""") ==
        Right(NamedValue(NamedValue.Argument, "clé", Some("DEFAULT"))))
  }
  "$returnCode" - {
    testExpression("$returnCode",
      LastReturnCode)
    testExpression("""variable("returnCode", label=LABEL)""",
      NamedValue(NamedValue.ByLabel("LABEL"), "returnCode"))
    testExpression("""variable("returnCode", job=JOB)""",
      NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), "returnCode"))
  }

  "Boolean" - {
    testBooleanExpression("true", true)
    testBooleanExpression("false", false)
    testBooleanExpression("(false)", false)
  }

  "ConstantString.quote" - {
    for string <- Seq("", " ", "'", "''", "\"", "\n", "A\nB", "\t", "\r") do
      testExpression(StringConstant.quote(string), string)

    "More characters" in:
      for i <- 0 to 0x200 do
        val string = i.toChar.toString
        testExpressionRaw(StringConstant.quote(string), string)
  }

  "String" - {
    testExpression("'x'", "x")
    testExpression("'ö'", "ö")
    testExpression("""'a\x'""", """a\x""")
    testExpression("""'a\\x'""", """a\\x""")
    testExpression(""" "" """.trim, "")
    testExpression(""" "x" """.trim, "x")
    testExpression(""" "ö" """.trim, "ö")

    "Escaping special characters" - {
      "Raw control characters are not escaped" in:
        for char <- ((0 until 0x20) ++ (0x7f until 0xa0)).map(_.toChar) do
          val doubleQuoted = s""" "$char" """.trim
          testExpressionRaw(doubleQuoted, s"$char")

          val singleQuoted = s""" '$char' """.trim
          testExpressionRaw(singleQuoted, s"$char")

      "U+0080...U+7FFF" in:
        for char <- (0x80 to 0x7fff).view.map(_.toChar) do
          val doubleQuoted = s""" "$char" """.trim
          testExpressionRaw(doubleQuoted, s"$char")
          val singleQuoted = s""" "'$char" """.trim
          testExpressionRaw(singleQuoted, s"'$char")

      val escapedChars = Seq(
        'n' -> '\n',
        'r' -> '\r',
        't' -> '\t',
        '"' -> '"',
        '$' -> '$',
        '\\' -> '\\')

      for (escaped, expected) <- escapedChars do
        testExpression(s""" "'\\$escaped" """.trim, s"'$expected")
        testExpression(s""" "\\$escaped" """.trim, s"$expected")

      "Single quoted string" - {
        testExpression("'ONE\nTWO'".trim, "ONE\nTWO")
        testExpression("'ONE\r\nTWO'".trim, "ONE\nTWO")

        // TODO Bad syntax, because ' cannot be used at start or end of the string
        testExpression("''->'<-''".trim, "->'<-")
        testExpression("'''->''<-'''".trim, "->''<-")
        testExpression("''''->'''<-''''".trim, "->'''<-")
        testExpression("'''''->''''<-'''''".trim, "->''''<-")
      }

      "Invalid escaped characters" in:
        val invalidEscaped = (0x20 to 0xff).map(_.toChar)
          .filter(c => c != '\r' && c != '\n')
          .filterNot(escapedChars.map(_._1).toSet)
        for escaped <- invalidEscaped do
          // With ' to render as "-string
          assert(parseExpression(s""" "'\\$escaped" """.trim) == Left(Problem(
            s"""Error in expression: Parsing failed at position 4 “"'\\❓$escaped"”""" +
            """ · Expected blackslash (\) and one of the following characters: [\"trn$]""")))

          // Without ' to render as '-string
          assert(parseExpression(s""" "\\$escaped" """.trim) == Left(Problem(
            s"""Error in expression: Parsing failed at position 3 “"\\❓$escaped"”""" +
            """ · Expected blackslash (\) and one of the following characters: [\"trn$]""")))
    }

    testExpression(""""A"""", "A")
    testExpression(""""$A$B"""", InterpolatedString(List(NamedValue("A"), NamedValue("B"))))
    testExpression(""""$`A:1`B"""", InterpolatedString(List(NamedValue("A:1"), "B")))
    testExpression(""""${A}B"""", InterpolatedString(List(NamedValue("A"), "B")))
    testExpression(""""${`A:1`}B"""", InterpolatedString(List(NamedValue("A:1"), "B")))
    testExpression(""""$A:B"""", InterpolatedString(List(NamedValue("A"), ":B")))

    "Interpolated string" in:
      assert(parseExpression(""""$A"""") == Right(InterpolatedString(NamedValue("A") :: Nil)))
      assert(parseExpression(""""-->$A$BB<--"""") ==
        Right(InterpolatedString(List("-->", NamedValue("A"), NamedValue("BB"), "<--"))))
      assert(parseExpression(""""-->${AB}${CD}<--"""") ==
        Right(InterpolatedString(List("-->", NamedValue("AB"), NamedValue("CD"), "<--"))))
      assert(parseExpression(""""-->$("A")<--"""") ==
        Right(StringConstant("-->A<--")))
      assert(parseExpression("""""""") == Right(StringConstant.empty))
      assert(parseExpression(""""\\\t\"\r\n"""") == Right(StringConstant("\\\t\"\r\n")))

    "Invalid strings" in:
      assert(parseExpression("''").isLeft)
      assert(parseExpression(""" "\" """.trim).isLeft)
      // We do not reject any string - assert(parseExpression(" \"\t\" ".trim).isLeft)
  }

  //TODO testError(""""1" < 1""",
  //  """Expected Expression is not of type String: '1' < 1:1:8, found """"")

  "Comparison" - {
    testBooleanExpression("$returnCode != 7",
      NotEqual(LastReturnCode, NumericConstant(7)))
    testBooleanExpression("$returnCode > 7",
      GreaterThan(LastReturnCode, NumericConstant(7)))
    testBooleanExpression("""$A == "X"""",
      Equal(NamedValue("A"), "X"))

    testBooleanExpression("$returnCode > 0 && $returnCode < 9",
      And(
        GreaterThan(LastReturnCode, NumericConstant(0)),
        LessThan(LastReturnCode, 9)))

    testBooleanExpression("$returnCode >= 0 && $returnCode <= 9",
      And(
        GreaterOrEqual(LastReturnCode, 0),
        LessOrEqual(LastReturnCode, 9)))

    testBooleanExpression("$returnCode == 1 || $returnCode == 2 || $returnCode == 3",
      Or(
        Or(
          Equal(LastReturnCode, 1),
          Equal(LastReturnCode, NumericConstant(2))),
        Equal(LastReturnCode, NumericConstant(3))))

    testBooleanExpression("""$returnCode >= 0 && $returnCode <= 9 && $result == "OK"""",
      And(
        And(
          GreaterOrEqual(LastReturnCode, NumericConstant(0)),
          LessOrEqual(LastReturnCode, 9)),
        Equal(NamedValue("result"), "OK")))

    testBooleanExpression("""$returnCode in [0, 3, 50]""",
      In(
        LastReturnCode,
        ListExpr.of(0, 3, 50)))

    // Boolean operand are no longer statically typed checked (via BooleanExpr),
    // to allow MissingValue.
    //testError("""($returnCode in [0, 3, 50]) || $result == "1"""",
    //  """Error in expression: Parsing failed at position 44 “…ult == "1"❓”""" +
    //   """ · Expected boolean operands for operator ||: [0, 3, 50] || $result == '1'""")

    testBooleanExpression("""($returnCode in [0, 3, 50]) || $result == "1"""",
      Or(
        In(
          LastReturnCode,
          ListExpr.of(0, 3, 50)),
        Equal(
          NamedValue("result"),
          "1")))

    testBooleanExpression("""$returnCode==$expected.toNumber|| !($result=="1")||true&&$returnCode>0""",
      Or(
        Or(
          Equal(
            LastReturnCode,
            ToNumber(NamedValue("expected"))),
          Not(Equal(
              NamedValue("result"),
              "1"))),
        And(
          True,
          GreaterThan(
            LastReturnCode,
            NumericConstant(0)))))

    testExpression("""["STRING", $NAME, 7].mkString""",
      MkString(ListExpr.of("STRING", NamedValue("NAME"), 7)))
  }

  testExpression("1+2",
    Add(1, NumericConstant(2)))

  testExpression("1 + 2 + 3",
    Add(Add(1, 2), 3))

  testExpression("'A' ++ 'B'++'C'",
    Concat(Concat("A", "B"), "C"))

  testExpression("'STRING'.stripMargin",
    StripMargin("STRING"))

  testBooleanExpression("""$result matches 'A.*'""",
    Matches(
      NamedValue("result"),
      "A.*"))

  "FunctionCall" - {
    testExpression("orderId", FunctionCall("orderId", None))
    testExpression("myFunction()", FunctionCall("myFunction", Some(Nil)))
    testExpression("myFunction(1)", FunctionCall("myFunction", Some(Seq(Argument(1)))))
    testExpression("myFunction(named=1, 'STRING')",
      FunctionCall(
        "myFunction",
        Some(Seq(
          Argument(1, Some("named")),
          Argument("STRING")))))
    testExpression("myFunction(nested())",
      FunctionCall(
        "myFunction",
        Some(Seq(
          Argument(FunctionCall("nested", Some(Nil)))))))
  }

  "Unknown numeric function" in:
    assert(parseExpression(""""123".toNumber""") ==
      Right(ToNumber("123")))

  "Unknown boolean function" in:
    assert(parseExpression(""""true".toBoolean""") ==
      Right(ToBoolean("true")))

  private def testBooleanExpression(exprString: String, expr: BooleanExpr)(using x: source.Position) =
    exprString in:
      assert(parseExpression(exprString) == Right(expr))
      assert(parseExpression(expr.toString) == Right(expr), " - toString")

  private def testExpression(exprString: String, expr: Expression)(using source.Position) =
    exprString in:
      testExpressionRaw(exprString, expr)

  private def testExpressionRaw(exprString: String, expr: Expression)(using source.Position) =
    assert(parseExpression(exprString) == Right(expr))
    assert(parseExpression(expr.toString) == Right(expr), " - toString")

  private def testError(exprString: String, errorMessage: String)(using source.Position) =
    exprString + " - should fail" in:
      testErrorRaw(exprString, errorMessage)

  private def testErrorRaw(exprString: String, errorMessage: String)(using source.Position) =
    assert(parseExpression(exprString) == Left(Problem(errorMessage)))
