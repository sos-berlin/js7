package js7.data_for_java.item

import js7.base.annotation.javaApi
import js7.data.item.{VersionedItem, VersionedItemPath}
import js7.data_for_java.common.JJsonable

@javaApi
trait JVersionedItem[A <: JVersionedItem[A, P], P <: VersionedItemPath]
extends JSignableItem with JJsonable[A]
{
  type AsScala <: VersionedItem

  def id: JVersionedItemId[P]
}
