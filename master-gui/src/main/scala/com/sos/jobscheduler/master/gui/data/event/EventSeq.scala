package com.sos.jobscheduler.master.gui.data.event

import com.sos.jobscheduler.master.gui.data.event.EventSeq._
import io.circe.{Decoder, HCursor}
import scala.collection.immutable.Seq
import scala.language.higherKinds

sealed trait TearableEventSeq[+M[_], +E]

sealed trait EventSeq[+M[_], +E] extends TearableEventSeq[M, E]

object EventSeq {
  final case class NonEmpty[M[_] <: Seq[_], E](stampeds: M[Stamped[E]])
  extends EventSeq[M, E] {
    assert(stampeds.nonEmpty)
  }

  object NonEmpty {
    implicit def jsonDecoder[E: Decoder]: Decoder[NonEmpty[Seq, E]] =
      (c: HCursor) ⇒
        for (eventSnapshots ← c.downField("eventSnapshots").as[Seq[Stamped[E]]]) yield
          NonEmpty(eventSnapshots)
  }

  final case class Empty(lastEventId: EventId)
  extends EventSeq[Nothing, Nothing]

  case object Torn
  extends TearableEventSeq[Nothing, Nothing]

  implicit def jsonDecoder[E: Decoder]: Decoder[EventSeq[Seq, E]] =
    (c: HCursor) ⇒
      for {
        typ ← c.downField("TYPE").as[String]
        eventSeq ← typ match {
          case "NonEmpty" ⇒ c.as[NonEmpty[Seq, E]]
          case "Empty" ⇒
            for (lastEventId ← c.downField("lastEventId").as[EventId]) yield
              Empty(lastEventId)
        }
      } yield eventSeq
}

object TearableEventSeq {
  private val TornDecoder = Decoder.const(Torn)

  implicit def jsonDecoder[E: Decoder]: Decoder[TearableEventSeq[Seq, E]] =
    (c: HCursor) ⇒
      for {
        typ ← c.downField("TYPE").as[String]
        eventSeq ← (if (typ == "Torn") TornDecoder else EventSeq.jsonDecoder[E]) apply c
      } yield eventSeq
}
