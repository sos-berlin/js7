package com.sos.scheduler.engine.data.compounds

import com.sos.scheduler.engine.data.event.EventId
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final case class SchedulerResponse[+A](content: A)(val eventId: EventId) {
  def map[B](f: A ⇒ B): SchedulerResponse[B] = SchedulerResponse(f(content))(eventId)
}

object SchedulerResponse {
  val EventIdJsonName = "eventId"
  val ContentJsonName = "schedulerResponseContent"

  implicit def jsonFormat[A: RootJsonFormat] = new RootJsonFormat[SchedulerResponse[A]] {

    def write(o: SchedulerResponse[A]) = {
      val contentFields = implicitly[RootJsonFormat[A]].write(o.content) match {
        case JsObject(fields) ⇒ fields
        case array: JsArray ⇒ Map(ContentJsonName → array)
        case x ⇒ sys.error(s"Unexpected ${x.getClass}")
      }
      JsObject(Map(EventIdJsonName → EventId.toJsValue(o.eventId)) ++ contentFields)   // eventId in content overrides
    }

    def read(jsValue: JsValue) = {
      val jsObject = jsValue.asJsObject
      val eventId = EventId.fromJsValue(jsObject.fields(EventIdJsonName))
      val content = jsObject.fields.getOrElse(ContentJsonName, jsObject)
      SchedulerResponse(content.convertTo[A])(eventId)
    }
  }

  def unwrapJsArray(jsObject: JsObject): JsArray = jsObject.fields(ContentJsonName).asInstanceOf[JsArray]
}
