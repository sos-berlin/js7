package js7.data_for_java.item

import js7.base.annotation.javaApi
import js7.data.item.{VersionedControl, VersionedControlPath}
import js7.data_for_java.common.JJsonable

@javaApi
trait JUnsignedVersionedItem[A <: JUnsignedVersionedItem[A, P], P <: VersionedControlPath]
extends JInventoryItem with JJsonable[A]:
  type AsScala <: VersionedControl

  def id: JUnsignedVersionedItemId[P]
