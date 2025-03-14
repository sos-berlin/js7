package js7.data.board

import js7.base.annotation.javaApi
import js7.base.generic.GenericString
import js7.data.item.UnsignedSimpleItemPath
import js7.data.plan.PlanId
import org.jetbrains.annotations.TestOnly

final case class BoardPath private(string: String) extends UnsignedSimpleItemPath:
  protected type Self = BoardPath

  val companion: BoardPath.type = BoardPath

  def /(plannedNoticeKey: PlannedNoticeKey): NoticeId =
    NoticeId(plannedNoticeKey.planId, this / plannedNoticeKey.noticeKey)

  def /(noticeKey: NoticeKey): BoardNoticeKey =
    BoardNoticeKey(this, noticeKey)

  @TestOnly
  def /(noticeKey: String): BoardNoticeKey =
    this / NoticeKey(noticeKey)

  /** A NoticeId in a GlobalBoard.
    */
  @TestOnly
  def \(noticeKey: String): NoticeId =
    this \ NoticeKey(noticeKey)

  /** A NoticeId in a GlobalBoard.
    */
  @TestOnly
  def \(noticeKey: NoticeKey): NoticeId =
    PlanId.Global / this / noticeKey


object BoardPath extends UnsignedSimpleItemPath.Companion[BoardPath]:
  type Item = GlobalBoard

  protected def unchecked(string: String) = new BoardPath(string)

  @javaApi @throws[RuntimeException]
  def of(validName: String): BoardPath =
    mayThrow(validName)

  given Ordering[BoardPath] = GenericString.ordering
