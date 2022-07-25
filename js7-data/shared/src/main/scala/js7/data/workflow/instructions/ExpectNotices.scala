package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.extras.Configuration.default.withDefaults
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.source.SourcePos

final case class ExpectNotices(
  boardPaths: BoardPathExpression,
  sourcePos: Option[SourcePos] = None)
extends BoardInstruction
{
  def withoutSourcePos = copy(sourcePos = None)

  def referencedBoardPaths = boardPaths.boardPaths

  def isFulfilled(isNoticeAvailable: BoardPath => Boolean): Boolean =
    boardPaths.eval(isNoticeAvailable)
}

object ExpectNotices
{
  implicit val jsonCodec: Codec.AsObject[ExpectNotices] = {
    implicit val configuration = withDefaults
    deriveConfiguredCodec[ExpectNotices]
  }
}
