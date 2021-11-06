package js7.data.agent

import js7.data.item.{InventoryItemPath, SimpleItemPath}

trait DelegateId extends SimpleItemPath

object DelegateId
{
  type Companion_ = Companion[_ <: DelegateId]

  trait Companion[A <: DelegateId]
  extends InventoryItemPath.Companion[A]
}
