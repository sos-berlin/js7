package com.sos.scheduler.engine.data.event

import spray.json._

/**
  * A value with an EventId.
  *
  * @author Joacim Zschimmer
  */
final case class Snapshot[+A](eventId: EventId, value: A) {

  def map[B](f: A ⇒ B): Snapshot[B] = Snapshot(eventId, f(value))

  def instant = EventId.toInstant(eventId)

  override def toString = s"Snapshot($instant $value)"
}

object Snapshot {
  val EventIdJsonName = "eventId"
  val ElementsJsonName = "elements"

  implicit def jsonFormat[A: RootJsonFormat]: RootJsonFormat[Snapshot[A]] =
    new RootJsonFormat[Snapshot[A]] {

      def write(o: Snapshot[A]) = {
        val contentFields = implicitly[RootJsonFormat[A]].write(o.value) match {
          case JsObject(fields) ⇒ fields
          case array: JsArray ⇒ Map(ElementsJsonName → array)
          case x ⇒ sys.error(s"Unexpected ${x.getClass}")
        }
        JsObject(contentFields + (EventIdJsonName → EventId.toJsValue(o.eventId)))   // eventId in content overrides
      }

      def read(jsValue: JsValue) = {
        val jsObject = jsValue.asJsObject
        val eventId = EventId.fromJsValue(jsObject.fields(EventIdJsonName))
        val content = jsObject.fields.getOrElse(ElementsJsonName, jsObject)
        Snapshot(eventId, content.convertTo[A])
      }
    }
}
