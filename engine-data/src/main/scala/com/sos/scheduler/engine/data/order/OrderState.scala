package com.sos.scheduler.engine.data.order

import com.fasterxml.jackson.annotation.JsonCreator
import com.sos.scheduler.engine.base.generic.IsString

final case class OrderState(string: String) extends IsString

object OrderState extends IsString.HasJsonFormat[OrderState] {
  @JsonCreator def jsonCreator(o: String) =
    new OrderState(o)
}
