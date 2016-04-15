package com.sos.scheduler.engine.data.order

import com.fasterxml.jackson.annotation.JsonCreator
import com.sos.scheduler.engine.base.generic.IsString

final case class OrderId(string: String) extends IsString


object OrderId {
  @JsonCreator def valueOf(string: String) = new OrderId(string)
}
