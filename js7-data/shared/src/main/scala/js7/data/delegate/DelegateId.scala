package js7.data.delegate

import js7.data.item.{SimpleItemPath, UnsignedSimpleItemPath}

trait DelegateId extends UnsignedSimpleItemPath

object DelegateId
{
  type Companion_ = Companion[? <: DelegateId]

  trait Companion[A <: DelegateId]
  extends SimpleItemPath.Companion[A]
}
