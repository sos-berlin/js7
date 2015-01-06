package com.sos.scheduler.engine.data.base

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.JsonSerializer
import com.fasterxml.jackson.databind.SerializerProvider

/** @author Joacim Zschimmer */
final class GenericIntSerializer extends JsonSerializer[GenericInt] {

  override def handledType = classOf[GenericInt]

  def serialize(o: GenericInt, g: JsonGenerator, p: SerializerProvider): Unit = {
    g.writeNumber(o.value)
  }
}
