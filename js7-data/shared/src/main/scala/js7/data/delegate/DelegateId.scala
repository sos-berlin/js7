package js7.data.delegate

import js7.data.item.SimpleItemPath

trait DelegateId extends SimpleItemPath

object DelegateId:
  type Companion_ = Companion[? <: DelegateId]

  trait Companion[A <: DelegateId]
  extends SimpleItemPath.Companion[A]
