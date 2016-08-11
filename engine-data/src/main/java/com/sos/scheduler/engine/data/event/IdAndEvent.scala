package com.sos.scheduler.engine.data.event

import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat
import com.sos.scheduler.engine.data.events.EventJsonFormat
import java.time.Instant
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final case class IdAndEvent(eventId: EventId, event: Event) {

  /**
    * Unique and nearly accurate timestamp.
    */
  def eventInstant: Instant = EventId.toInstant(eventId)
}

object IdAndEvent {
  implicit val IdAndEventJsonFormat = new RootJsonFormat[IdAndEvent] {
    def write(o: IdAndEvent) = JsObject(o.event.toJson.asJsObject.fields + ("eventId" → JsNumber(o.eventId)))

    def read(json: JsValue) = IdAndEvent(
      eventId = json.asJsObject.fields("eventId").asInstanceOf[JsNumber].value.toLongExact,
      event = json.convertTo[Event])
  }

  def canSerialize(o: IdAndEvent)(implicit typedJsonFormat: TypedJsonFormat[Event]) =
    typedJsonFormat canSerialize o.event
}
