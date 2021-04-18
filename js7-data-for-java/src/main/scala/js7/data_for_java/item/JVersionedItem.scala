package  js7.data_for_java.item

import js7.base.annotation.javaApi
import js7.data.item.{ItemPath, VersionedItem}
import js7.data_for_java.common.JJsonable

@javaApi
trait JVersionedItem[A <: JVersionedItem[A, P], P <: ItemPath]
extends JSignableItem with JJsonable[A]
{
  protected type AsScala <: VersionedItem

  def id: JItemId[P]
}
