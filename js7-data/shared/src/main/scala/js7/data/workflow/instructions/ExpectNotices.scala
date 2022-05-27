package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.generic.extras.JsonKey
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.source.SourcePos

final case class ExpectNotices(
  @JsonKey("boardPaths")
  boardPathExpr: BoardPathExpression,
  sourcePos: Option[SourcePos] = None)
extends BoardInstruction
{
  def withoutSourcePos = copy(sourcePos = None)

  def referencedBoardPaths = boardPathExpr.boardPaths

  def isFulfilled(isNoticeAvailable: BoardPath => Boolean): Boolean =
    boardPathExpr.eval(isNoticeAvailable)
}

object ExpectNotices
{
  implicit val jsonCodec: Codec.AsObject[ExpectNotices] = {
    implicit val configuration = withDefaults
    deriveConfiguredCodec[ExpectNotices]
  }
}
