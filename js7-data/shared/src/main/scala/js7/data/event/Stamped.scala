package js7.data.event

import cats.{Eq, Functor}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, JsonObject}
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.Stamped.*
import scala.collection.mutable

/**
  * A value with an EventId.
  *
  * @author Joacim Zschimmer
  */
final case class Stamped[+A](eventId: EventId, timestampMillis: Long, value: A):
  // Delay Timesstamp construction for faster JSON deserialization
  def timestamp: Timestamp =
    Timestamp.ofEpochMilli(timestampMillis)

  def map[B](f: A => B): Stamped[B] =
    functor.map(this)(f)

  override def toString =
    s"Stamped($eventId $timestamp $value)"


object Stamped:
  def apply[A](eventId: EventId, value: A): Stamped[A] =
    new Stamped(eventId, EventId.toEpochMilli(eventId), value)

  def apply[A](eventId: EventId, timestamp: Timestamp, value: A): Stamped[A] =
    new Stamped(eventId, timestamp.toEpochMilli, value)

  def checkOrdering[A](lastEventId: EventId, stampedSeq: IterableOnce[Stamped[A]])
  : Checked[Unit] =
    var checked = Checked.unit
    var last = lastEventId
    val iterator = stampedSeq.iterator

    while iterator.hasNext && checked.isRight do
      val eventId = iterator.next().eventId
      if eventId <= last then
        checked = Left(Problem.pure(
          if eventId == last then
            s"Duplicate EventId ${EventId.toString(eventId)}"
          else
            s"EventId ${EventId.toString(eventId)} <= ${EventId.toString(last)}"))
      last = eventId

    checked

  implicit def stampedEq[A: Eq]: Eq[Stamped[A]] = Eq.fromUniversalEquals

  implicit val functor: Functor[Stamped] = new Functor[Stamped]:
    def map[A,B](fa: Stamped[A])(f: A => B) =
      fa.copy(value = f(fa.value))

  implicit def jsonEncoder[A: Encoder]: Encoder.AsObject[Stamped[A]] =
    stamped => {
      val fields = mutable.Buffer.empty[(String, Json)]
      fields += "eventId" -> Json.fromLong(stamped.eventId)
      val epochMilli = stamped.timestampMillis
      if epochMilli != EventId.toEpochMilli(stamped.eventId) then
        fields += "timestamp" -> Json.fromLong(epochMilli)
      val json = stamped.value.asJson
      json.asObject match
        case Some(o) =>
          fields ++= o.toIterable
        case None =>
          if !json.isArray then sys.error(
            "Stamped[A]: The A type must serialize to a JSON object or array, " +
            s"but not: ${json.getClass.shortClassName}")
          fields += "array" -> json
      JsonObject.fromIterable(fields)
    }

  implicit def jsonDecoder[A: Decoder]: Decoder[Stamped[A]] =
    cursor =>
      for
        eventId <- cursor.get[EventId]("eventId")
        timestampMillis <- cursor.getOrElse[Long]("timestamp")(EventId.toEpochMilli(eventId))
        a <-
          val arr = cursor.downField("array")
          // stamped.value must not contain a field named "array" !!!
          if arr.succeeded then
            arr.as[A]
          else
            cursor.as[A]
      yield Stamped(eventId, timestampMillis, a)
