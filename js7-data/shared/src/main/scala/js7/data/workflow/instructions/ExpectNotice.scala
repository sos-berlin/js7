package js7.data.workflow.instructions

import io.circe.generic.semiauto.deriveDecoder
import js7.base.circeutils.typed.Subtype
import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.source.SourcePos

private object ExpectNotice:
  // COMPATIBLE with v2.3
  private final case class ExpectNotice(
    boardPath: BoardPath,
    sourcePos: Option[SourcePos] = None)

  val compatibleSubtype: Subtype[ExpectNotices] =
    Subtype.decodeCompatible(deriveDecoder[ExpectNotice])(expectNotice =>
      Right(ExpectNotices(
        BoardPathExpression.ExpectNotice(expectNotice.boardPath),
        expectNotice.sourcePos)))
