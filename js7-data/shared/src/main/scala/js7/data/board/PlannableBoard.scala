package js7.data.board

import io.circe.Codec
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.item.ItemRevision
import js7.data.value.expression.Scope

/** A PlannableBoard is a BoardItem with NoticeIds for each PlanId.
  */
final case class PlannableBoard(
  path: BoardPath,
  itemRevision: Option[ItemRevision] = None)
extends
  BoardItem:

  protected type Self = PlannableBoard

  val companion: PlannableBoard.type = PlannableBoard

  def rename(path: BoardPath): PlannableBoard =
    copy(path = path)

  def withRevision(revision: Option[ItemRevision]): PlannableBoard =
    copy(itemRevision = revision)

  def isGlobal: Boolean =
    false

  //def isAnnounced: Boolean =
  //  true

  protected def evalEndOfLife(scope: Scope): Checked[Option[Timestamp]] =
    Right(None)

object PlannableBoard extends BoardItem.Companion[PlannableBoard]:

  type ItemState = BoardState

  val Path: BoardPath.type = BoardPath

  def cls: Class[PlannableBoard] =
    classOf[PlannableBoard]

  given jsonCodec: Codec.AsObject[PlannableBoard] = deriveConfiguredCodec
