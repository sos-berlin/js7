package js7.data.board

import io.circe.{Decoder, Encoder, Json}
import js7.base.circeutils.CirceUtils.*
import js7.data.value.ValuePrinter.quoteString
import js7.data.value.expression.{HasPrecedence, Precedence}

sealed trait BoardPathExpression extends HasPrecedence:
  def eval(isNoticeAvailable: BoardPath => Boolean): Boolean

  def boardPaths: Set[BoardPath]


object BoardPathExpression:
  final case class ExpectNotice(boardPath: BoardPath)
  extends BoardPathExpression:
    def precedence: Int = Precedence.Factor

    val boardPaths: Set[BoardPath] = Set(boardPath)

    def eval(isNoticeAvailable: BoardPath => Boolean): Boolean =
      isNoticeAvailable(boardPath)

    override def toString: String = quoteString(boardPath.string)

  final case class And(a: BoardPathExpression, b: BoardPathExpression)
  extends BoardPathExpression:
    def precedence: Int = Precedence.And

    val boardPaths: Set[BoardPath] = a.boardPaths ++ b.boardPaths

    def eval(isNoticeAvailable: BoardPath => Boolean): Boolean =
      a.eval(isNoticeAvailable) && b.eval(isNoticeAvailable)

    override def toString: String = makeString(a, "&&", b)

  final case class Or(a: BoardPathExpression, b: BoardPathExpression)
  extends BoardPathExpression:
    def precedence: Int = Precedence.Or

    val boardPaths: Set[BoardPath] = a.boardPaths ++ b.boardPaths

    def eval(isNoticeAvailable: BoardPath => Boolean): Boolean =
      a.eval(isNoticeAvailable) || b.eval(isNoticeAvailable)

    override def toString: String = makeString(a, "||", b)

  implicit val jsonEncoder: Encoder[BoardPathExpression] =
    o => Json.fromString(o.toString)

  implicit val jsonDecoder: Decoder[BoardPathExpression] =
    c => for
      expr <- c.as[String]
      expr <- BoardPathExpressionParser.parse(expr).toDecoderResult(c.history)
    yield expr
