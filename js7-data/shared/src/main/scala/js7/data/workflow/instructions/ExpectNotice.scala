package js7.data.workflow.instructions

import io.circe.generic.semiauto.deriveDecoder
import js7.base.circeutils.typed.Subtype
import js7.data.board.BoardPath
import js7.data.source.SourcePos

// COMPATIBLE with v2.3
@deprecated("Use ExpectNotices", "2.4")
final case class ExpectNotice(
  boardPath: BoardPath,
  sourcePos: Option[SourcePos] = None)

object ExpectNotice
{
  val compatibleSubtype: Subtype[ExpectNotices] =
    Subtype.decodeCompatible(deriveDecoder[ExpectNotice])(expectNotice =>
      ExpectNotices(Vector(expectNotice.boardPath), expectNotice.sourcePos))
}
