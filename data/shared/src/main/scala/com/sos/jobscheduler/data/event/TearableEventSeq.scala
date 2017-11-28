package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.objectCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.event.EventSeq._
import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, JsonObject}
import scala.collection.immutable.Seq
import scala.language.higherKinds

/**
  * @author Joacim Zschimmer
  */
sealed trait TearableEventSeq[+M[_], +E]

sealed trait EventSeq[+M[_], +E] extends TearableEventSeq[M, E]

object EventSeq {
  final case class NonEmpty[M[_] <: TraversableOnce[_], E](stampeds: M[Stamped[E]])
  extends EventSeq[M, E] {
    assert(stampeds.nonEmpty)
  }

  @JsonCodec
  final case class Empty(lastEventId: EventId)
  extends EventSeq[Nothing, Nothing]

  case object Torn
  extends TearableEventSeq[Nothing, Nothing] {
    implicit val JsonCodec = objectCodec(Torn)
  }

  implicit def nonEmptyJsonEncoder[E: Encoder]: Encoder[NonEmpty[Seq, E]] =
    eventSeq ⇒ Json.fromJsonObject(JsonObject.fromMap(Map("eventSnapshots" → eventSeq.stampeds.asJson)))

  implicit def nonEmptyJsonDecoder[E: Decoder]: Decoder[NonEmpty[Seq, E]] =
    _.get[Seq[Stamped[E]]]("eventSnapshots") map NonEmpty.apply

  implicit def jsonCodec[E: Encoder: Decoder]: CirceCodec[EventSeq[Seq, E]] =
    TypedJsonCodec[EventSeq[Seq, E]](
      Subtype[NonEmpty[Seq, E]],
      Subtype[Empty])
}

object TearableEventSeq {

  implicit def jsonCodec[E: Encoder: Decoder]: CirceCodec[TearableEventSeq[Seq, E]] =
    TypedJsonCodec[TearableEventSeq[Seq, E]](
      Subtype[EventSeq[Seq, E]],
      Subtype[Torn.type])
}
