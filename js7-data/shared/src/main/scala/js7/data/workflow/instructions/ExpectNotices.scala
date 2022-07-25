package js7.data.workflow.instructions

import io.circe.Codec
import js7.base.circeutils.CirceUtils.{DecodeWithDefaults, deriveConfiguredCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport
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
  implicit val jsonCodec: Codec.AsObject[ExpectNotices] = deriveConfiguredCodec[ExpectNotices]

  intelliJuseImport(DecodeWithDefaults)
}
