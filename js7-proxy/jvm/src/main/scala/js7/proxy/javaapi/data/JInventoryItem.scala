package js7.proxy.javaapi.data

import js7.base.annotation.javaApi
import js7.data.item.{InventoryItem, TypedPath}

@javaApi
trait JInventoryItem[A <: JInventoryItem[A, P], P <: TypedPath]
extends JJsonable[A]
{
  protected type Underlying <: InventoryItem

  def id: JItemId[P]
}
