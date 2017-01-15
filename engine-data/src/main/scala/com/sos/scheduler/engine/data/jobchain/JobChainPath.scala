package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.filebased.TypedPath
import com.sos.scheduler.engine.data.order.{OrderId, OrderKey}

final case class JobChainPath(string: String)
extends TypedPath {

  validate()

  def companion = JobChainPath

  def orderKey(o: String): OrderKey = orderKey(OrderId(o))

  def orderKey(o: OrderId): OrderKey = new OrderKey(this, o)
}


object JobChainPath extends TypedPath.Companion[JobChainPath] {

  override protected[engine] def isCommaAllowed = false
}
