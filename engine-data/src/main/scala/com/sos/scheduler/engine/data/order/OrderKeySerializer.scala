package com.sos.scheduler.engine.data.order

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.JsonSerializer
import com.fasterxml.jackson.databind.SerializerProvider

/** @author Joacim Zschimmer */
final class OrderKeySerializer extends JsonSerializer[OrderKey] {

  override def handledType =
    classOf[OrderKey]

  def serialize(o: OrderKey, g: JsonGenerator, p: SerializerProvider): Unit = {
    g.writeStartObject()
    g.writeStringField("jobChainPath", o.jobChainPath.string)
    g.writeStringField("id", o.id.string)
    g.writeEndObject()
  }
}
