package com.sos.jobscheduler.master.gui.data.event

import io.circe.{Decoder, HCursor}

/**
  * @author Joacim Zschimmer
  */
final case class Stamped[A](eventId: EventId, value: A) {
  def map[B](f: A ⇒ B): Stamped[B] =
    Stamped(eventId, f(value))
}

object Stamped {
  implicit def jsonDecoder[A: Decoder]: Decoder[Stamped[A]] =
    (c: HCursor) ⇒ {
      val stampedResult = c.downField("elements").success match {
        case Some(cursor) ⇒ cursor.as[A]
        case None ⇒ c.as[A]
      }
      for {
        stamped ← stampedResult
        eventId ← c.downField("eventId").as[EventId]
      } yield Stamped(eventId, stamped)
    }
}
