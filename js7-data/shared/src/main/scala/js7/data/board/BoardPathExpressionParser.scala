package js7.data.board

import cats.parse.Parser
import cats.parse.Parser.string
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.BoardPathExpression.{And, ExpectNotice, Or}
import js7.data.parser.CatsBasicParsers._
import js7.data.parser.CatsParsers.checkedParse
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
    path(BoardPath).map(ExpectNotice)

  private val and: Parser[BoardPathExpression] =
    leftRecurse[BoardPathExpression, Unit](expectNotice, string("&&"), expectNotice) {
      case (a, ((), b)) => And(a, b)
    }

  private val or: Parser[BoardPathExpression] =
    leftRecurse(and, string("||"), and) {
      case (a, ((), b)) => Or(a, b)
    }

  private val noticeExpression: Parser[BoardPathExpression] =
    or
}
