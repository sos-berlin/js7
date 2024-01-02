package js7.data.job

import cats.parse.Parser.{char, charWhere, charsWhile0, end, failWith, stringIn}
import cats.parse.{Parser, Parser0}
import js7.base.parser.BasicParsers.singleQuoted
import js7.base.parser.Parsers
import js7.base.problem.Checked
import js7.data.job.CommandLineExpression.optimizeCommandLine
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.{ListExpr, MkString, StringConstant, StringExpr}
import js7.data.value.expression.ExpressionParser.dollarNamedValue

object CommandLineParser:
  def parse(source: String): Checked[CommandLineExpression] =
    Parsers.checkedParse(
      source,
      commandLine
        .map(CommandLineExpression(source, _))
        .map(optimizeCommandLine))

  private val maybeWhite: Parser0[Unit] =
    charsWhile0(isWhite).void

  private val escapedCharInUnquotedWord: Parser[String] =
    stringIn(List("""\\""", """\"""", """\'""", """\$"""))
      .map(_.tail)

  private val normalCharInUnquotedWord: Parser[Char] =
    charWhere(c => !isWhite(c) && c != '$' && c != '"' && c != '\'' && c != '\\')

  private val stringConstant: Parser[StringConstant] =
    (normalCharInUnquotedWord.map(_.toString) | escapedCharInUnquotedWord).rep
      .map(chars => StringConstant(chars.toList.mkString))

  private val constantInDoubleQuotedWord: Parser0[StringConstant] =
    charsWhile0(c => c != '\\' && c != '"' && c != '$')
      .map(StringConstant.apply)

  private val escapedInDoubleQuotedWord: Parser[StringExpr] =
    stringIn(List("""\\""", """\"""", """\$"""))
      .map(o => StringConstant(o.tail))

  private val doubleQuotedWord: Parser[StringExpr] =
    (char('"') ~
      constantInDoubleQuotedWord ~
      ((escapedInDoubleQuotedWord | dollarNamedValue) ~ constantInDoubleQuotedWord).rep0 ~
      char('"')
    ) .map { case ((((), constant), tail), ()) =>
        MkString(ListExpr(
          constant :: tail.flatMap { case (a, b) => a :: b :: Nil }))
      }

  private val singleQuotedWord: Parser[StringExpr] =
    singleQuoted
      .map(StringConstant.apply)

  private val word: Parser[StringExpr] =
    ((stringConstant | doubleQuotedWord | singleQuotedWord | dollarNamedValue).rep(1))
      .map(exprs => MkString(ListExpr(exprs.toList)))

  private val firstWord: Parser[StringExpr] =
    (!end).orElse(failWith("The command line must not be empty")).with1 *>
      word

  private val commandLine: Parser0[List[Expression]] =
    (maybeWhite ~ firstWord ~ maybeWhite ~ (word ~ maybeWhite).rep0 ~ maybeWhite)
      .map { case (((((), head), ()), tail), ()) => head :: tail.map(_._1) }

  private def isWhite(c: Char) =
    c.toInt >= 0 && c <= ' '
