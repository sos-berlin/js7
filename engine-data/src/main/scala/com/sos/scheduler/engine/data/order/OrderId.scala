package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.base.IsString
import com.fasterxml.jackson.annotation.JsonCreator

final case class OrderId(string: String) extends IsString


object OrderId {
  @JsonCreator def valueOf(string: String) = new OrderId(string)
}
