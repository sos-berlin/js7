package com.sos.scheduler.engine.data.jobchain

import com.fasterxml.jackson.annotation.JsonCreator
import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}
import com.sos.scheduler.engine.data.order.{OrderId, OrderKey}

final case class JobChainPath(string: String)
extends TypedPath {

  validate()
  require(!(name contains ","), "JobChainPath must not contain a comma (because this denotes an OrderKey)")

  def companion = JobChainPath

  def orderKey(o: String): OrderKey = orderKey(OrderId(o))

  def orderKey(o: OrderId): OrderKey = new OrderKey(this, o)
}


object JobChainPath extends TypedPath.Companion[JobChainPath] {

  @JsonCreator def valueOf(absolutePath: String) = new JobChainPath(absolutePath)

  // 'def' due to mutual singleton dependency of this and FileBasedType
  def fileBasedType = FileBasedType.JobChain
}
