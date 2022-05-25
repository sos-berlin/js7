package js7.data.board

import io.circe.{Decoder, Encoder, Json}
import js7.base.circeutils.CirceUtils._
import js7.data.value.ValuePrinter.quoteString
import js7.data.value.expression.Precedence

sealed trait BoardPathExpression extends Precedence
{
  def eval(isNoticeAvailable: BoardPath => Boolean): Boolean

  def boardPaths: Set[BoardPath]
}

object BoardPathExpression
{
  final case class ExpectNotice(boardPath: BoardPath)
  extends BoardPathExpression
  {
    def precedence = Precedence.Factor

    val boardPaths = Set(boardPath)

    def eval(isNoticeAvailable: BoardPath => Boolean) =
      isNoticeAvailable(boardPath)

    override def toString = quoteString(boardPath.string)
  }

  final case class And(a: BoardPathExpression, b: BoardPathExpression)
  extends BoardPathExpression {
    def precedence = Precedence.And

    val boardPaths = a.boardPaths ++ b.boardPaths

    def eval(isNoticeAvailable: BoardPath => Boolean) =
      a.eval(isNoticeAvailable) && b.eval(isNoticeAvailable)

    override def toString = toString(a, "&&", b)
  }

  final case class Or(a: BoardPathExpression, b: BoardPathExpression)
  extends BoardPathExpression {
    def precedence = Precedence.Or

    val boardPaths = a.boardPaths ++ b.boardPaths

    def eval(isNoticeAvailable: BoardPath => Boolean) =
      a.eval(isNoticeAvailable) || b.eval(isNoticeAvailable)

    override def toString = toString(a, "||", b)
  }

  implicit val jsonEncoder: Encoder[BoardPathExpression] =
    o => Json.fromString(o.toString)

  implicit val jsonDecoder: Decoder[BoardPathExpression] =
    c => for {
      expr <- c.as[String]
      expr <- BoardPathExpressionParser.parse(expr).toDecoderResult(c.history)
    } yield expr
}
