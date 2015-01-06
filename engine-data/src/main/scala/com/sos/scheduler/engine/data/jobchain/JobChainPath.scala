package com.sos.scheduler.engine.data.jobchain

import com.fasterxml.jackson.annotation.JsonCreator
import com.sos.scheduler.engine.data.base.IsString
import com.sos.scheduler.engine.data.filebased.{AbsolutePath, FileBasedType, TypedPath}
import com.sos.scheduler.engine.data.order.OrderId
import com.sos.scheduler.engine.data.order.OrderKey

final case class JobChainPath(string: String)
extends TypedPath {

  requireIsAbsolute()

  def fileBasedType = FileBasedType.jobChain

  def orderKey(o: String): OrderKey = orderKey(OrderId(o))

  def orderKey(o: OrderId): OrderKey = new OrderKey(this, o)
}


object JobChainPath extends IsString.HasJsonFormat[JobChainPath] {
  @JsonCreator def valueOf(absolutePath: String) = new JobChainPath(absolutePath)

  def makeAbsolute(path: String) =
    new JobChainPath(AbsolutePath.makeAbsolute(path))
}
