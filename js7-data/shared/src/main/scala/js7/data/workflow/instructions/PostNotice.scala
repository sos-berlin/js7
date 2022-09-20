package js7.data.workflow.instructions

import io.circe.generic.semiauto.deriveDecoder
import js7.base.circeutils.typed.Subtype
import js7.data.board.BoardPath
import js7.data.source.SourcePos

// COMPATIBLE with v2.3
@deprecated("Use PostNotices", "2.4")
final case class PostNotice(
  boardPath: BoardPath,
  sourcePos: Option[SourcePos] = None)

object PostNotice
{
  val compatibleSubtype: Subtype[PostNotices] =
    Subtype.decodeCompatible(deriveDecoder[PostNotice])(postNotice =>
      Right(PostNotices(Vector(postNotice.boardPath), postNotice.sourcePos)))
}
