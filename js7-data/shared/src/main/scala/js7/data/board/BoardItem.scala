package js7.data.board

import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.board.BoardItem.*
import js7.data.item.UnsignedSimpleItem
import js7.data.value.expression.Scope

trait BoardItem extends UnsignedSimpleItem:

  protected type Self <: BoardItem

  val companion: Companion[Self]

  final def toInitialItemState =
    BoardState(this)

  def toNotice(noticeId: NoticeId, endOfLife: Option[Timestamp] = None)(scope: Scope)
  : Checked[Notice]


object BoardItem:

  trait Companion[I <: BoardItem] extends UnsignedSimpleItem.Companion[I]:
    type Key = BoardPath

    def Key = BoardPath

    type ItemState = BoardState
