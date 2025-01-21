package js7.data.board

import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.board.BoardItem.*
import js7.data.item.UnsignedSimpleItem
import js7.data.value.expression.Scope

trait BoardItem extends UnsignedSimpleItem:

  protected type Self <: BoardItem

  val companion: Companion[Self]

  def path: BoardPath

  def isGlobal: Boolean

  //def isAnnounced: Boolean

  protected def evalEndOfLife(scope: Scope): Checked[Option[Timestamp]]

  final def toInitialItemState: BoardState =
    BoardState(this)

  def toNotice(plannedNoticeKey: PlannedNoticeKey, endOfLife: Option[Timestamp] = None)
    (scope: Scope)
  : Checked[Notice] =
    for endOfLife <- endOfLife.fold(evalEndOfLife(scope))(o => Checked(Some(o))) yield
      Notice(path / plannedNoticeKey, endOfLife)


object BoardItem:

  trait Companion[I <: BoardItem] extends UnsignedSimpleItem.Companion[I]:
    type Key = BoardPath

    def Key = BoardPath

    type ItemState = BoardState
