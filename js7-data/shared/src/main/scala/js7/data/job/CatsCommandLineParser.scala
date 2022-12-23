package js7.data.job

import cats.parse.Parser.{char, charWhere, charsWhile0, end, failWith, stringIn}
import cats.parse.{Parser, Parser0}
import js7.base.problem.Checked
import js7.data.job.CommandLineExpression.optimizeCommandLine
import js7.data.parser.CatsBasicParsers.singleQuoted
import js7.data.parser.CatsParsers
import js7.data.value.expression.CatsExpressionParser.dollarNamedValue
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.{ListExpression, MkString, StringConstant, StringExpression}

object CatsCommandLineParser
{
  def parse(source: String): Checked[CommandLineExpression] =
    CatsParsers.checkedParse(
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

  private val escapedInDoubleQuotedWord: Parser[StringExpression] =
    stringIn(List("""\\""", """\"""", """\$"""))
      .map(o => StringConstant(o.tail))

  private val doubleQuotedWord: Parser[StringExpression] =
    (char('"') ~
      constantInDoubleQuotedWord ~
      ((escapedInDoubleQuotedWord | dollarNamedValue) ~ constantInDoubleQuotedWord).rep0 ~
      char('"')
    ) .map { case ((((), constant), tail), ()) =>
        MkString(ListExpression(
          constant :: tail.flatMap { case (a, b) => a :: b :: Nil }))
      }

  private val singleQuotedWord: Parser[StringExpression] =
    singleQuoted
      .map(StringConstant.apply)

  private val word: Parser[StringExpression] =
    ((stringConstant | doubleQuotedWord | singleQuotedWord | dollarNamedValue).rep(1))
      .map(exprs => MkString(ListExpression(exprs.toList)))

  private val firstWord: Parser[StringExpression] =
    (!end).orElse(failWith("The command line must not be empty")).with1 *>
      word

  private val commandLine: Parser0[List[Expression]] =
    (maybeWhite ~ firstWord ~ maybeWhite ~ (word ~ maybeWhite).rep0 ~ maybeWhite)
      .map { case (((((), head), ()), tail), ()) => head :: tail.map(_._1) }

  private def isWhite(c: Char) =
    c.toInt >= 0 && c <= ' '
}
