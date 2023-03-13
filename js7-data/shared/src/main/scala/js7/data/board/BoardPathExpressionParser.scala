package js7.data.board

import cats.parse.Parser
import cats.parse.Parser.string
import js7.base.parser.BasicParsers.*
import js7.base.parser.Parsers.checkedParse
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.BoardPathExpression.{And, ExpectNotice, Or}
import js7.data.parser.Js7Parsers.path
import org.jetbrains.annotations.TestOnly

object BoardPathExpressionParser
{
  @TestOnly
  def boardPathExpr(string: String): BoardPathExpression =
    parse(string).orThrow

  def parse(expr: String): Checked[BoardPathExpression] =
    checkedParse(expr, noticeExpression)

  private val expectNotice: Parser[ExpectNotice] =
    path(BoardPath).map(ExpectNotice(_))

  private val factor: Parser[BoardPathExpression] =
    inParentheses(noticeExpression) | expectNotice

  private val and: Parser[BoardPathExpression] =
    leftRecurse[BoardPathExpression, Unit](factor, string("&&"), factor) {
      case (a, ((), b)) => And(a, b)
    }

  private val or: Parser[BoardPathExpression] =
    leftRecurse(and, string("||"), and) {
      case (a, ((), b)) => Or(a, b)
    }

  private lazy val noticeExpression: Parser[BoardPathExpression] =
    Parser.defer(or)
}
