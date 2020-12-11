package js7.proxy.javaapi.data.item

import js7.base.annotation.javaApi
import js7.data.item.{ItemPath, VersionedItem}
import js7.proxy.javaapi.data.common.JJsonable

@javaApi
trait JVersionedItem[A <: JVersionedItem[A, P], P <: ItemPath]
extends JJsonable[A]
{
  protected type AsScala <: VersionedItem

  def id: JItemId[P]
}
