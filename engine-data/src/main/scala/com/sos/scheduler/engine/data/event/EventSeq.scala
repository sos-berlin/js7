package com.sos.scheduler.engine.data.event

import com.sos.scheduler.engine.base.sprayjson.SprayJson.implicits.RichJsValue
import scala.collection.immutable.Seq
import scala.language.higherKinds
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
trait EventSeq[+M[_], +E]

object EventSeq {

  final case class NonEmpty[M[_], E](events: M[Snapshot[E]])
  extends EventSeq[M, E]

  final case class Empty(lastEventId: EventId)
  extends EventSeq[Nothing, Nothing]

  case object Teared
  extends EventSeq[Nothing, Nothing]

  implicit def jsonFormat[E: RootJsonFormat] = new RootJsonFormat[EventSeq[Seq, E]] {
    def write(o: EventSeq[Seq, E]) = o match {
      case NonEmpty(events) ⇒ JsObject("events" → events.toJson)
      case Empty(lastEventId) ⇒ JsObject("lastEventId" → JsNumber(lastEventId))
      case Teared ⇒ JsObject()
    }

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      fields.get("events") match {
        case Some(events) ⇒
          NonEmpty(events.asJsArray.elements map { _.convertTo[Snapshot[E]] })
        case None ⇒
          fields.get("lastEventId") match {
            case Some(lastEventId) ⇒
              Empty(lastEventId.asJsNumber.value.toLongExact)
            case None ⇒
              Teared
          }
      }
    }
  }
}
