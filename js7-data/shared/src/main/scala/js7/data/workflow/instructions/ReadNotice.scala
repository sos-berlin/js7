package js7.data.workflow.instructions

import io.circe.generic.semiauto.deriveCodec
import js7.data.board.BoardPath
import js7.data.source.SourcePos

final case class ReadNotice(
  boardPath: BoardPath,
  sourcePos: Option[SourcePos] = None)
extends BoardInstruction
{
  def withoutSourcePos = copy(sourcePos = None)
}

object ReadNotice
{
  implicit val jsonCodec = deriveCodec[ReadNotice]
}
