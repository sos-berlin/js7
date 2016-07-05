package com.sos.scheduler.engine.base.generic

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}

/** @author Joacim Zschimmer */
final class GenericIntSerializer extends JsonSerializer[GenericInt] {

  override def handledType = classOf[GenericInt]

  def serialize(o: GenericInt, g: JsonGenerator, p: SerializerProvider): Unit = {
    g.writeNumber(o.number)
  }
}
