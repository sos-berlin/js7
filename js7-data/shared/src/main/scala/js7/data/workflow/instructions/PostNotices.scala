package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.board.BoardPath
import js7.data.source.SourcePos
import js7.data.workflow.Instruction

final case class PostNotices private(
  boardPaths: Vector[BoardPath],
  sourcePos: Option[SourcePos])
extends BoardInstruction:

  val referencedBoardPaths: Set[BoardPath] = boardPaths.toSet

  protected def checked: Checked[this.type] =
    for _ <- boardPaths.checkUniqueness yield this

  def withoutSourcePos: Instruction = copy(sourcePos = None)


object PostNotices:
  def apply(
    boardPaths: Iterable[BoardPath],
    sourcePos: Option[SourcePos] = None)
  : PostNotices =
    checked(boardPaths.toVector, sourcePos).orThrow

  def checked(
    boardPaths: Vector[BoardPath],
    sourcePos: Option[SourcePos] = None)
  : Checked[PostNotices] =
    for _ <- boardPaths.checkUniqueness yield
      new PostNotices(boardPaths, sourcePos)

  implicit val jsonCodec: Codec.AsObject[PostNotices] =
    deriveCodec[PostNotices].checked(_.checked)
