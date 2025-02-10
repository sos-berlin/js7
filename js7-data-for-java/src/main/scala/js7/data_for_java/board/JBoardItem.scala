package js7.data_for_java.board

import java.util.Optional
import javax.annotation.Nonnull
import js7.data.board.{BoardItem, BoardPath, GlobalBoard, PlannableBoard, PlannedBoard}
import js7.data.item.ItemRevision
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem

trait JBoardItem
extends JJsonable[? <: JBoardItem], JUnsignedSimpleItem:

  type AsScala <: BoardItem
  val asScala: AsScala

  @Nonnull
  def path: BoardPath

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]): JBoardItem


object JBoardItem:

  def apply(boardItem: BoardItem): JBoardItem =
    boardItem match
      case item: GlobalBoard => JGlobalBoard(item)
      case item: PlannableBoard => JPlannableBoard(item)
