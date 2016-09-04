package com.sos.scheduler.engine.data.event

import com.sos.scheduler.engine.base.sprayjson.SprayJson.implicits.RichJsValue
import spray.json._

/**
  * A value with an EventId.
  *
  * @author Joacim Zschimmer
  */
final case class Snapshot[+A](value: A)(val eventId: EventId) {

  def map[B](f: A ⇒ B): Snapshot[B] = Snapshot(f(value))(eventId)

  def instant = EventId.toInstant(eventId)

  override def toString = s"Snapshot($instant $value)"
}

object Snapshot {
  val EventIdJsonName = "eventId"
  val ValueJsonName = "value"

  implicit def jsonFormat[A: RootJsonFormat] = new RootJsonFormat[Snapshot[A]] {

    def write(o: Snapshot[A]) = {
      val contentFields = implicitly[RootJsonFormat[A]].write(o.value) match {
        case JsObject(fields) ⇒ fields
        case array: JsArray ⇒ Map(ValueJsonName → array)
        case x ⇒ sys.error(s"Unexpected ${x.getClass}")
      }
      JsObject(Map(EventIdJsonName → EventId.toJsValue(o.eventId)) ++ contentFields)   // eventId in content overrides
    }

    def read(jsValue: JsValue) = {
      val jsObject = jsValue.asJsObject
      val eventId = EventId.fromJsValue(jsObject.fields(EventIdJsonName))
      val content = jsObject.fields.getOrElse(ValueJsonName, jsObject)
      Snapshot(content.convertTo[A])(eventId)
    }
  }

  def unwrapJsArray(jsObject: JsObject): JsArray = jsObject.fields(ValueJsonName).asJsArray
}
