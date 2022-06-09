package js7.data.value.expression

import cats.parse.Parser.end
import js7.base.problem.Problem
import js7.data.parser.CatsParsers.checkedParse
import js7.data.value.expression.CatsExpressionParser.{parse => _, _}
import js7.data.value.expression.Expression._
import js7.data.workflow.instructions.executable.WorkflowJob
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec

final class CatsExpressionParserTest extends AnyFreeSpec
{
  // See also ExpressionTest

  "NamedValue" - {
    "$ with impossible names" in {
      assert(dollarNamedValue.parse("$var/1") == Right("/1" -> NamedValue("var")))
      assert(dollarNamedValue.parse("$var.1") == Right(".1" -> NamedValue("var")))
      assert(dollarNamedValue.parse("$var-1") == Right("-1" -> NamedValue("var")))
      assert(dollarNamedValue.parse("$var_1") == Right("" -> NamedValue("var_1")))
    }

    testExpression("""$key""", NamedValue("key"))
    testExpression("""$under_line""", NamedValue("under_line"))
    testExpression("""$Schlüssel""", NamedValue("Schlüssel"))
    testExpression("""$clé""", NamedValue("clé"))
    testExpression("""$A""", NamedValue("A"))
    testExpression("""${SOME_KEY}""", NamedValue("SOME_KEY"))
    testExpression("""$`weird name, with dot., and comma`""", NamedValue("weird name, with dot., and comma"))
    testExpression("""${`weird name, with dot., and comma`}""", NamedValue("weird name, with dot., and comma"))
    testExpression(""""${`weird name, with dot., and comma`}"""", InterpolatedString(List(NamedValue("weird name, with dot., and comma"))))
    //testExpression("""${arg::SOME-KEY}""", NamedValue(NamedValue.Argument, StringConstant("SOME-KEY")))
    //testExpression("""${label::LABEL.SOME-KEY}""", NamedValue(NamedValue.ByLabel(Label("LABEL")), StringConstant("SOME-KEY")))
    //testExpression("""${job::JOB.SOME-KEY}""", NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), StringConstant("SOME-KEY")))
    //testExpression("""${A.SOME-KEY}""", NamedValue(NamedValue.LastOccurredByPrefix("A"), StringConstant("SOME-KEY")))

    "variable()" in {
      assert(checkedParse("""variable("clé")""", expression) ==
        Right(NamedValue("clé")))
      assert(checkedParse("""variable ( "clé", default = "DEFAULT" )""", expression) ==
        Right(NamedValue("clé", StringConstant("DEFAULT"))))
      assert(checkedParse("""variable(key="clé", label=LABEL)""", expression) ==
        Right(NamedValue(NamedValue.ByLabel("LABEL"), StringConstant("clé"))))
      assert(checkedParse("""variable(key="clé", job=JOB)""", expression) ==
        Right(NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), StringConstant("clé"))))
    }

    "argument()" in {
      assert(checkedParse("""argument("clé")""", expression) ==
        Right(NamedValue(NamedValue.Argument, StringConstant("clé"))))
      assert(checkedParse("""argument ( "clé", default = "DEFAULT" )""", expression) ==
        Right(NamedValue(NamedValue.Argument, StringConstant("clé"), Some(StringConstant("DEFAULT")))))
    }
  }

  "$returnCode" - {
    testExpression("$returnCode",
      LastReturnCode)
    testExpression("""variable("returnCode", label=LABEL)""",
      NamedValue(NamedValue.ByLabel("LABEL"), StringConstant("returnCode")))
    testExpression("""variable("returnCode", job=JOB)""",
      NamedValue(NamedValue.LastExecutedJob(WorkflowJob.Name("JOB")), StringConstant("returnCode")))
  }

  testExpression("catchCount", OrderCatchCount)

  "Boolean" - {
    testBooleanExpression("true", BooleanConstant(true))
    testBooleanExpression("false", BooleanConstant(false))
    testBooleanExpression("(false)", BooleanConstant(false))
  }

  "ConstantString.quote" - {
    for (string <- Seq("", " ", "'", "''", "\"", "\n", "A\nB", "\t", "\r")) {
      testExpression(StringConstant.quote(string), StringConstant(string))
    }

    "More characters" in {
      for (i <- 0 to 0x200) {
        val string = i.toChar.toString
        testExpressionRaw(StringConstant.quote(string), StringConstant(string))
      }
    }
  }

  "String" - {
    testExpression("'x'", StringConstant("x"))
    testExpression("'ö'", StringConstant("ö"))
    testExpression("""'a\x'""", StringConstant("""a\x"""))
    testExpression("""'a\\x'""", StringConstant("""a\\x"""))
    testExpression(""" "" """.trim, StringConstant(""))
    testExpression(""" "x" """.trim, StringConstant("x"))
    testExpression(""" "ö" """.trim, StringConstant("ö"))

    "Escaping special characters" - {
      "Raw control characters are not escaped" in {
        for (char <- ((0 until 0x20) ++ (0x7f until 0xa0)).map(_.toChar)) {
          val doubleQuoted = s""" "$char" """.trim
          testExpressionRaw(doubleQuoted, StringConstant(s"$char"))

          val singleQuoted = s""" '$char' """.trim
          testExpressionRaw(singleQuoted, StringConstant(s"$char"))
        }
      }

      "U+0080...U+7FFF" in {
        for (char <- (0x80 to 0x7fff).view.map(_.toChar)) {
          val doubleQuoted = s""" "$char" """.trim
          testExpressionRaw(doubleQuoted, StringConstant(s"$char"))
          val singleQuoted = s""" "'$char" """.trim
          testExpressionRaw(singleQuoted, StringConstant(s"'$char"))
        }
      }

      val escapedChars = Seq(
        'n' -> '\n',
        'r' -> '\r',
        't' -> '\t',
        '"' -> '"',
        '$' -> '$',
        '\\' -> '\\')

      for ((escaped, expected) <- escapedChars) {
        testExpression(s""" "'\\$escaped" """.trim, StringConstant(s"'$expected"))
        testExpression(s""" "\\$escaped" """.trim, StringConstant(s"$expected"))
      }

      "Single quoted string" - {
        testExpression(s"'ONE\nTWO'".trim, StringConstant("ONE\nTWO"))
        testExpression(s"'ONE\r\nTWO'".trim, StringConstant("ONE\nTWO"))

        // TODO Bad syntax, because ' cannot be used at start or end of the string
        testExpression(s"''->'<-''".trim, StringConstant("->'<-"))
        testExpression(s"'''->''<-'''".trim, StringConstant("->''<-"))
        testExpression(s"''''->'''<-''''".trim, StringConstant("->'''<-"))
        testExpression(s"'''''->''''<-'''''".trim, StringConstant("->''''<-"))
      }

      "Invalid escaped characters" in {
        val invalidEscaped = (0x20 to 0xff).map(_.toChar)
          .filter(c => c != '\r' && c != '\n')
          .filterNot(escapedChars.map(_._1).toSet)
        for (escaped <- invalidEscaped) {
          // With ' to render as "-string
          assert(CatsExpressionParser.parse(s""" "'\\$escaped" """.trim) == Left(Problem(
            s"""Error in expression: Parsing failed at position 4 “"'\\❓$escaped"”""" +
            """ · Expected blackslash (\) and one of the following characters: [\"trn$]""")))

          // Without ' to render as '-string
          assert(CatsExpressionParser.parse(s""" "\\$escaped" """.trim) == Left(Problem(
            s"""Error in expression: Parsing failed at position 3 “"\\❓$escaped"”""" +
            """ · Expected blackslash (\) and one of the following characters: [\"trn$]""")))
        }
      }
    }

    testExpression(""""A"""", StringConstant("A"))
    testExpression(""""$A$B"""", InterpolatedString(List(NamedValue("A"), NamedValue("B"))))
    testExpression(""""$`A:1`B"""", InterpolatedString(List(NamedValue("A:1"), StringConstant("B"))))
    testExpression(""""${A}B"""", InterpolatedString(List(NamedValue("A"), StringConstant("B"))))
    testExpression(""""${`A:1`}B"""", InterpolatedString(List(NamedValue("A:1"), StringConstant("B"))))
    testExpression(""""$A:B"""", InterpolatedString(List(NamedValue("A"), StringConstant(":B"))))

    "Interpolated string" in {
      assert(checkedParse(""""$A"""", expression) == Right(InterpolatedString(NamedValue("A") :: Nil)))
      assert(checkedParse(""""-->$A$BB<--"""", expression) ==
        Right(InterpolatedString(List(StringConstant("-->"), NamedValue("A"), NamedValue("BB"), StringConstant("<--")))))
      assert(checkedParse(""""-->${AB}${CD}<--"""", expression) ==
        Right(InterpolatedString(List(StringConstant("-->"), NamedValue("AB"), NamedValue("CD"), StringConstant("<--")))))
      assert(checkedParse(""""-->$("A")<--"""", expression) ==
        Right(StringConstant("-->A<--")))
      assert(checkedParse("""""""", expression) == Right(StringConstant.empty))
      assert(checkedParse(""""\\\t\"\r\n"""", expression) == Right(StringConstant("\\\t\"\r\n")))
    }

    "Invalid strings" in {
      assert(checkedParse("''", expression).isLeft)
      assert(checkedParse(""" "\" """.trim, expression).isLeft)
      // We do not reject any string - assert(checkedParse(" \"\t\" ".trim, expression).isLeft)
    }
  }

  //TODO testError(""""1" < 1""",
  //  """Expected Expression is not of type String: '1' < 1:1:8, found """"")

  "Comparison" - {
    testBooleanExpression("$returnCode != 7",
      NotEqual(LastReturnCode, NumericConstant(7)))
    testBooleanExpression("$returnCode > 7",
      GreaterThan(LastReturnCode, NumericConstant(7)))
    testBooleanExpression("""variable("A") == "X"""",
      Equal(NamedValue("A"), StringConstant("X")))
    testBooleanExpression("""$A == "X"""",
      Equal(NamedValue("A"), StringConstant("X")))

    testBooleanExpression("$returnCode > 0 && $returnCode < 9",
      And(
        GreaterThan(LastReturnCode, NumericConstant(0)),
        LessThan(LastReturnCode, NumericConstant(9))))

    testBooleanExpression("$returnCode >= 0 && $returnCode <= 9",
      And(
        GreaterOrEqual(LastReturnCode, NumericConstant(0)),
        LessOrEqual(LastReturnCode, NumericConstant(9))))

    testBooleanExpression("$returnCode == 1 || $returnCode == 2 || $returnCode == 3",
      Or(
        Or(
          Equal(LastReturnCode, NumericConstant(1)),
          Equal(LastReturnCode, NumericConstant(2))),
        Equal(LastReturnCode, NumericConstant(3))))

    testBooleanExpression("""$returnCode >= 0 && $returnCode <= 9 && $result == "OK"""",
      And(
        And(
          GreaterOrEqual(LastReturnCode, NumericConstant(0)),
          LessOrEqual(LastReturnCode, NumericConstant(9))),
        Equal(NamedValue("result"), StringConstant("OK"))))

    testBooleanExpression("""$returnCode in [0, 3, 50]""",
      In(
        LastReturnCode,
        ListExpression(List(NumericConstant(0), NumericConstant(3), NumericConstant(50)))))

    testError("""$returnCode in [0, 3, 50] || $result == "1"""",
      """Parsing failed at position 44 “…ult == "1"❓”""" +
       """ · Expected boolean operans for operator ||: [0, 3, 50] || $result == '1'""")

    testBooleanExpression("""($returnCode in [0, 3, 50]) || $result == "1"""",
      Or(
        In(
          LastReturnCode,
          ListExpression(List(NumericConstant(0), NumericConstant(3), NumericConstant(50)))),
        Equal(
          NamedValue("result"),
          StringConstant("1"))))

    testBooleanExpression("""$returnCode==$expected.toNumber||!($result=="1")||true&&$returnCode>0""",
      Or(
        Or(
          Equal(
            LastReturnCode,
            ToNumber(NamedValue("expected"))),
          Not(Equal(
              NamedValue("result"),
              StringConstant("1")))),
        And(
          BooleanConstant(true),
          GreaterThan(
            LastReturnCode,
            NumericConstant(0)))))

    testExpression("""["STRING", $NAME, 7].mkString""",
      MkString(ListExpression(StringConstant("STRING") :: NamedValue("NAME") :: NumericConstant(7) :: Nil)))
  }

  testExpression("1+2",
    Add(NumericConstant(1), NumericConstant(2)))

  testExpression("1 + 2 + 3",
    Add(Add(NumericConstant(1), NumericConstant(2)), NumericConstant(3)))

  testExpression("'A' ++ 'B'++'C'",
    Concat(Concat(StringConstant("A"), StringConstant("B")), StringConstant("C")))

  testExpression("'STRING'.stripMargin",
    StripMargin(StringConstant("STRING")))

  testBooleanExpression("""$result matches 'A.*'""",
    Matches(
      NamedValue("result"),
      StringConstant("A.*")))

  "FunctionCall" - {
    testExpression("myFunction()", FunctionCall("myFunction"))
    testExpression("myFunction(1)", FunctionCall("myFunction", Seq(Argument(NumericConstant(1)))))
    testExpression("myFunction(named=1, 'STRING')",
      FunctionCall(
        "myFunction",
        Seq(
          Argument(NumericConstant(1), Some("named")),
          Argument(StringConstant("STRING")))))
    testExpression("myFunction(nested())",
      FunctionCall(
        "myFunction",
        Seq(
          Argument(FunctionCall("nested")))))
  }

  "Unknown numeric function" in {
    val parser = expression <* end
    assert(checkedParse(""""123".toNumber""", parser) ==
      Right(ToNumber(StringConstant("123"))))
  }

  "Unknown boolean function" in {
    val parser = expression <* end
    assert(checkedParse(""""true".toBoolean""", parser) ==
      Right(ToBoolean(StringConstant("true"))))
  }

  private def testBooleanExpression(exprString: String, expr: BooleanExpression)(implicit pos: source.Position) =
    registerTest(exprString) {
      val parser = expression <* end
      assert(checkedParse(exprString, parser) == Right(expr))
      assert(checkedParse(expr.toString, parser) == Right(expr), " - toString")
    }

  private def testExpression(exprString: String, expr: Expression)(implicit pos: source.Position) =
    registerTest(exprString) {
      testExpressionRaw(exprString, expr)
    }

  private def testExpressionRaw(exprString: String, expr: Expression)(implicit pos: source.Position) = {
    val parser = expression <* end
    assert(checkedParse(exprString, parser) == Right(expr))
    assert(checkedParse(expr.toString, parser) == Right(expr), " - toString")
  }

  private def testError(exprString: String, errorMessage: String)(implicit pos: source.Position) =
    registerTest(exprString + " - should fail") {
      testErrorRaw(exprString, errorMessage)
    }

  private def testErrorRaw(exprString: String, errorMessage: String)(implicit pos: source.Position) = {
    val parser = expression <* end
    assert(checkedParse(exprString, parser) == Left(Problem(errorMessage)))
  }
}