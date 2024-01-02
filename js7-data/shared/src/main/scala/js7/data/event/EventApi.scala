package js7.data.event

import io.circe.Decoder
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.web.{HttpClient, Uri}
import cats.effect.IO
import fs2.Stream

trait EventApi
extends SessionApi, HasIsIgnorableStackTrace:

  type State <: JournaledState[State]

  def baseUri: Uri

  def eventStream[E <: Event](request: EventRequest[E])
    (implicit kd: Decoder[KeyedEvent[E]])
  : IO[Stream[IO, Stamped[KeyedEvent[E]]]]

  final def checkedSnapshot(eventId: Option[EventId] = None): IO[Checked[State]] =
    HttpClient.liftProblem(snapshot(eventId))

  def snapshot(eventId: Option[EventId] = None): IO[State]
