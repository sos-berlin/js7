package js7.data.job

import fastparse.*
import fastparse.NoWhitespace.*
import js7.base.problem.Checked
import js7.data.job.CommandLineExpression.optimizeCommandLine
import js7.data.parser.FastparseBasicParsers.singleQuoted
import js7.data.parser.FastparseParsers
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.{ListExpression, MkString, StringConstant, StringExpression}
import js7.data.value.expression.FastparseExpressionParser.dollarNamedValue

object FastparseCommandLineParser
{
  def parse(source: String): Checked[CommandLineExpression] =
    FastparseParsers.checkedParse(source,
      commandLine(_).map(CommandLineExpression(source, _)).map(optimizeCommandLine))

  private def commandLine[x: P]: P[List[Expression]] = P(
    (wordBoundary.? ~~/ firstWord ~~/ (wordBoundary ~~ word).rep ~~/ wordBoundary.?)
      .map { case (head, tail) => head :: tail.toList })

  private def firstWord[x: P]: P[StringExpression] = P(
    (End ~/ Pass).flatMap(_ => Fail.opaque("The command line must not be empty")) |
    word)

  private def word[x: P]: P[StringExpression] = P(
    ((stringConstant | doubleQuotedWord | singleQuotedWord | dollarNamedValue).rep(1))
      .map(exprs => MkString(ListExpression(exprs.toList))))

  private def stringConstant[x: P]: P[StringConstant] = P(
    (normalCharInUnquotedWord | escapedCharInUnquotedWord).rep(1)
      .map(chars => StringConstant(chars.mkString)))

  private def escapedCharInUnquotedWord[x: P]: P[String] = P(
    ("""\\""" | """\"""" | """\'""" | """\$""").!
      .map(_.tail))

  private def normalCharInUnquotedWord[x: P]: P[Char] = P(
    CharPred(c => !isWhite(c) && c != '$' && c != '"' && c != '\'' && c != '\\').!.map(_.head))

  private def wordBoundary[x: P]: P[Unit] = P(
    CharsWhile(isWhite))

  private def isWhite(c: Char) = c >= 0 && c <= ' '

  private def doubleQuotedWord[x: P]: P[StringExpression] = P(
    ("\"" ~~/
      constantInDoubleQuotedWord ~~/
      ((escapedInDoubleQuotedWord | dollarNamedValue) ~~/ constantInDoubleQuotedWord).rep ~~/
      "\""
    ) .map { case (constant, tail) =>
        MkString(ListExpression(
          constant ::
            tail.flatMap { case (a, b) => a :: b :: Nil }.toList))
      })

  private def singleQuotedWord[x: P]: P[StringExpression] = P(
    singleQuoted
      .map(StringConstant.apply))

  private def constantInDoubleQuotedWord[x: P]: P[StringConstant] = P(
    CharsWhile(c => c != '\\' && c != '"' && c != '$', 0).!
      .map(StringConstant.apply))

  private def escapedInDoubleQuotedWord[x: P]: P[StringExpression] = P(
    ("""\\""" | """\"""" | """\$""").!
      .map(o => StringConstant(o.tail)))
}
