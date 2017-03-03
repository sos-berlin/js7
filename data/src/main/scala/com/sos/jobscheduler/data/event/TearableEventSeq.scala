package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.data.event.EventSeq._
import scala.collection.immutable.Seq
import scala.language.higherKinds
import spray.json.DefaultJsonProtocol._
import spray.json._

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

  private implicit def nonEmptyJsonFormat[E: RootJsonFormat]: RootJsonFormat[NonEmpty[Seq, E]] =
    //Does not compile: jsonFormat1((stampeds: Seq[Stamped[E]]) ⇒ NonEmpty(stampeds)), // Error: kinds of the type arguments (scala.collection.immutable.Seq) do not conform to the expected kinds of the type parameters (type T) ...
    new RootJsonFormat[NonEmpty[Seq, E]] {
      def write(o: NonEmpty[Seq, E]) = JsObject("eventSnapshots" → o.stampeds.toJson)
      def read(json: JsValue) = NonEmpty(json.asJsObject.fields("eventSnapshots").convertTo[Seq[Stamped[E]]])
    }

  final case class Empty(lastEventId: EventId)
  extends EventSeq[Nothing, Nothing]

  case object Torn
  extends TearableEventSeq[Nothing, Nothing]

  // TODO May be slow if instantiated often
  implicit def eventSeqJsonFormat[E: RootJsonFormat]: TypedJsonFormat[EventSeq[Seq, E]] =
    TypedJsonFormat[EventSeq[Seq, E]](
      Subtype[NonEmpty[Seq, E]],
      Subtype(jsonFormat1(Empty.apply)))
}

object TearableEventSeq {
  // TODO May be slow if instantiated often
  implicit def tearableEventSeqJsonFormat[E: RootJsonFormat]: TypedJsonFormat[TearableEventSeq[Seq, E]] =
    TypedJsonFormat[TearableEventSeq[Seq, E]](
      Subtype[EventSeq[Seq, E]],
      Subtype(jsonFormat0(() ⇒ Torn)))
}
