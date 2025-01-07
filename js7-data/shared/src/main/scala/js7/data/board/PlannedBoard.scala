package js7.data.board

import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.plan.PlanId

/** PlannedBoard, mirrors NoticeIds whose originals are stored in BoardState.
  */
final case class PlannedBoard(
  id: PlannedBoardId,
  keyToNoticePlace: Map[NoticeKey, NoticePlace]):

  def updateNoticePlace(noticePlace: NoticePlace): PlannedBoard =
    copy(keyToNoticePlace = keyToNoticePlace.updated(noticePlace.noticeId.noticeKey, noticePlace))

  def deleteNoticePlace(noticeKey: NoticeKey): Option[PlannedBoard] =
    val x = keyToNoticePlace - noticeKey
    x.nonEmpty ? copy(keyToNoticePlace = x)

  def planId: PlanId =
    id.planId

  def boardPath: BoardPath =
    id.boardPath

  def isEmpty: Boolean =
    keyToNoticePlace.isEmpty

  override def toString =
    s"PlannedBoard($id ${keyToNoticePlace.values.mkString(", ")})"


object PlannedBoard:

  def apply(id: PlannedBoardId, noticePlaces: Iterable[NoticePlace] = Nil): PlannedBoard =
    new PlannedBoard(id, noticePlaces.toKeyedMap(_.noticeId.noticeKey))

  given Ordering[PlannedBoard] = Ordering.by(_.id)
