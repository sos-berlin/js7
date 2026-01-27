package js7.data.workflow.instructions

import io.circe.derivation.ConfiguredCodec
import io.circe.{Codec, Decoder, Encoder}
import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.ExpectNotices.*
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced

final case class ExpectNotices(
  boardPaths: BoardPathExpression,
  whenNotAnnounced: WhenNotAnnounced = WhenNotAnnounced.Wait,
  sourcePos: Option[SourcePos] = None)
extends ExpectOrConsumeNoticesInstruction, Instruction.NoInstructionBlock:

  def withoutSourcePos: ExpectNotices =
    copy(sourcePos = None)

  def referencedBoardPaths: Set[BoardPath] =
    boardPaths.boardPaths


object ExpectNotices:

  given Codec.AsObject[ExpectNotices] = ConfiguredCodec.derive(useDefaults = true)
