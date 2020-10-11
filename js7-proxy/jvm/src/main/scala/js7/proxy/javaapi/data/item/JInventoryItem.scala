package js7.proxy.javaapi.data.item

import js7.base.annotation.javaApi
import js7.data.item.{InventoryItem, ItemPath}
import js7.proxy.javaapi.data.common.JJsonable

@javaApi
trait JInventoryItem[A <: JInventoryItem[A, P], P <: ItemPath]
extends JJsonable[A]
{
  protected type AsScala <: InventoryItem

  def id: JItemId[P]
}
