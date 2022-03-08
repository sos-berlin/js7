package js7.data.orderwatch

import js7.base.annotation.javaApi
import js7.data.item.{InventoryItemPath, UnsignedSimpleItemPath}

final case class OrderWatchPath(string: String)
extends UnsignedSimpleItemPath
with InventoryItemPath.AttachableToAgent
{
  protected type Self = OrderWatchPath

  val companion = OrderWatchPath
}

object OrderWatchPath extends UnsignedSimpleItemPath.Companion[OrderWatchPath]
{
  @javaApi
  def of(id: String) = apply(id)

  override protected def unchecked(string: String) =
    new OrderWatchPath(string)
}
