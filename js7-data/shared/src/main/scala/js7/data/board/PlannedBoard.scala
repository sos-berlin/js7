package js7.data.board

import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.plan.PlanId

/** PlannedBoard, mirrors NoticeIds whose originals are stored in BoardState.
  *
  * A PlannedBoard mirrors NoticeIds of a PlannableBoard or a GlobalBoard.
  */
final case class PlannedBoard(
  id: PlannedBoardId,
  noticeKeys: Set[NoticeKey]):

  def addNoticeKey(noticeKey: NoticeKey): PlannedBoard =
    if noticeKeys(noticeKey) then
      this
    else
      copy(noticeKeys = noticeKeys + noticeKey)

  def deleteNoticeKey(noticeKey: NoticeKey): Option[PlannedBoard] =
    val plannedBoard =
      if !noticeKeys(noticeKey) then
        this
      else
        copy(noticeKeys = noticeKeys - noticeKey)
    !plannedBoard.isEmpty ? plannedBoard

  def planId: PlanId =
    id.planId

  def boardPath: BoardPath =
    id.boardPath

  def isEmpty: Boolean =
    noticeKeys.isEmpty

  override def toString =
    s"PlannedBoard($id ${noticeKeys.toArray.sorted.mkString(", ")})"


object PlannedBoard:

  private val emptyNoticeKeySet = Set(NoticeKey.empty)

  def apply(id: PlannedBoardId, noticeKeys: Iterable[NoticeKey] = Nil): PlannedBoard =
    var noticeKeysSet = noticeKeys.toSet
    if noticeKeysSet == emptyNoticeKeySet then
      noticeKeysSet = emptyNoticeKeySet // reuse memory
    new PlannedBoard(id, noticeKeysSet)

  given Ordering[PlannedBoard] = Ordering.by(_.id)
