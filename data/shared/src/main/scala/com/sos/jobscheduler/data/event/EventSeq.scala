package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.CirceObjectCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}
import scala.collection.immutable.Seq
import scala.language.higherKinds

/**
  * @author Joacim Zschimmer
  */
sealed trait EventSeq[+M[_], +E] extends TearableEventSeq[M, E]

object EventSeq {
  final case class NonEmpty[M[_] <: TraversableOnce[_], E](stampeds: M[Stamped[E]])
  extends EventSeq[M, E] {
    assert(stampeds.nonEmpty)
  }

  @JsonCodec
  final case class Empty(lastEventId: EventId)
  extends EventSeq[Nothing, Nothing]

  implicit def nonEmptyJsonEncoder[E: ObjectEncoder]: ObjectEncoder[NonEmpty[Seq, E]] =
    eventSeq ⇒ JsonObject.singleton("stampeds", eventSeq.stampeds.asJson)

  implicit def nonEmptyJsonDecoder[E: Decoder]: Decoder[NonEmpty[Seq, E]] =
    _.get[Seq[Stamped[E]]]("stampeds") map NonEmpty.apply

  implicit def jsonCodec[E: ObjectEncoder: Decoder]: CirceObjectCodec[EventSeq[Seq, E]] =
    TypedJsonCodec[EventSeq[Seq, E]](
      Subtype[NonEmpty[Seq, E]],
      Subtype[Empty])
}

sealed trait TearableEventSeq[+M[_], +E]

object TearableEventSeq {
  /** Requested event is no longer available.
    * `oldestKnownEventId` is for testing only.
    * @param oldestKnownEventId
    */
  @JsonCodec
  final case class Torn(oldestKnownEventId: EventId)
  extends TearableEventSeq[Nothing, Nothing]

  implicit def jsonCodec[E: ObjectEncoder: Decoder]: CirceObjectCodec[TearableEventSeq[Seq, E]] =
    TypedJsonCodec[TearableEventSeq[Seq, E]](
      Subtype[EventSeq[Seq, E]],
      Subtype[Torn])
}
