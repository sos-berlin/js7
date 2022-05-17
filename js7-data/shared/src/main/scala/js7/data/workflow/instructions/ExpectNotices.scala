package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.CirceUtils.RichCirceCodec
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.board.BoardPath
import js7.data.source.SourcePos

final case class ExpectNotices(
  boardPaths: Vector[BoardPath],
  sourcePos: Option[SourcePos])
extends BoardInstruction
{
  def withoutSourcePos = copy(sourcePos = None)
}

object ExpectNotices
{
  def apply(
    boardPaths: Iterable[BoardPath],
    sourcePos: Option[SourcePos] = None)
  : ExpectNotices =
    checked(boardPaths.toVector, sourcePos).orThrow

  def checked(
    boardPaths: Vector[BoardPath],
    sourcePos: Option[SourcePos] = None)
  : Checked[ExpectNotices] =
    for (_ <- boardPaths.checkUniqueness) yield
      new ExpectNotices(boardPaths, sourcePos)

  implicit val jsonCodec: Codec.AsObject[ExpectNotices] = {
    val codec = deriveCodec[ExpectNotices]
    Codec.AsObject.from(codec.checked(_.checked), codec)
  }
}
