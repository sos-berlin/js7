package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import io.circe.{Decoder, Encoder, Json, JsonObject}
import io.circe.syntax.EncoderOps

/**
  * A value with an EventId.
  *
  * @author Joacim Zschimmer
  */
final case class Stamped[+A](eventId: EventId, value: A) {

  def map[B](f: A ⇒ B): Stamped[B] = Stamped(eventId, f(value))

  def instant = EventId.toInstant(eventId)

  override def toString = s"Stamped($instant $value)"
}

object Stamped {
  val EventIdJsonName = "eventId"
  val ElementsJsonName = "elements"

  implicit def jsonEncoder[A: Encoder]: Encoder[Stamped[A]] =
    stamped ⇒ {
      val json = stamped.value.asJson
      if (json.isArray)
        Json.fromJsonObject(JsonObject.fromMap(Map(
          EventIdJsonName → Json.fromLong(stamped.eventId),
          ElementsJsonName → json)))
      else
        Json.fromJsonObject(
          json.forceObject
            .add(EventIdJsonName, Json.fromLong(stamped.eventId)))
    }

  implicit def jsonDecoder[A: Decoder]: Decoder[Stamped[A]] =
    cursor ⇒
      for {
        eventId ← cursor.get[EventId](EventIdJsonName)
        a ← cursor.get[A]("elements") match {
          case o if o.isRight ⇒ o
          case _ ⇒ cursor.as[A]
        }
      } yield Stamped(eventId, a)
}
