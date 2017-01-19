package com.sos.scheduler.engine.data.event

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import scala.collection.immutable.Seq
import scala.language.higherKinds
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
trait EventSeq[+M[_], +E]

object EventSeq {

  final case class NonEmpty[M[_] <: TraversableOnce[_], E](eventSnapshots: M[Snapshot[E]])
  extends EventSeq[M, E] {
    assert(eventSnapshots.nonEmpty)
  }

  private implicit def nonEmptyJsonFormat[E: RootJsonFormat]: RootJsonFormat[NonEmpty[Seq, E]] =
    //Does not compile: jsonFormat1((eventSnapshots: Seq[Snapshot[E]]) ⇒ NonEmpty(eventSnapshots)), // Error: kinds of the type arguments (scala.collection.immutable.Seq) do not conform to the expected kinds of the type parameters (type T) ...
    new RootJsonFormat[NonEmpty[Seq, E]] {
      def write(o: NonEmpty[Seq, E]) = JsObject("eventSnapshots" → o.eventSnapshots.toJson)
      def read(json: JsValue) = NonEmpty(json.asJsObject.fields("eventSnapshots").convertTo[Seq[Snapshot[E]]])
    }

  final case class Empty(lastEventId: EventId)
  extends EventSeq[Nothing, Nothing]

  case object Torn
  extends EventSeq[Nothing, Nothing]

  implicit def jsonFormat[E: RootJsonFormat]: TypedJsonFormat[EventSeq[Seq, E]] =
    TypedJsonFormat[EventSeq[Seq, E]](
      Subtype[NonEmpty[Seq, E]],
      Subtype(jsonFormat1(Empty.apply)),
      Subtype(jsonFormat0(() ⇒ Torn)))
}
