package js7.data.agent

import js7.data.item.SimpleItemPath

trait DelegateId extends SimpleItemPath

object DelegateId
{
  type Companion_ = Companion[_ <: DelegateId]

  trait Companion[A <: DelegateId]
  extends SimpleItemPath.Companion[A]
}
