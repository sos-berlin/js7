package js7.data.board

import js7.base.annotation.javaApi
import js7.data.item.UnsignedSimpleItemPath

final case class BoardPath private(string: String) extends UnsignedSimpleItemPath:
  protected type Self = BoardPath

  val companion: BoardPath.type = BoardPath

object BoardPath extends UnsignedSimpleItemPath.Companion[BoardPath]:
  // May deadlock: override val itemTypeName = Board.typeName
  type Item = Board

  protected def unchecked(string: String) = new BoardPath(string)

  @javaApi
  def of(validName: String): BoardPath =
    apply(validName)
