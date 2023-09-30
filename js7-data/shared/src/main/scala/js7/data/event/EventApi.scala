package js7.data.event

import io.circe.Decoder
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.web.{HttpClient, Uri}
import monix.eval.Task
import monix.reactive.Observable

trait EventApi
extends SessionApi
with HasIsIgnorableStackTrace:

  type State <: JournaledState[State]

  def baseUri: Uri

  def eventObservable[E <: Event](request: EventRequest[E])
    (implicit kd: Decoder[KeyedEvent[E]])
  : Task[Observable[Stamped[KeyedEvent[E]]]]

  final def checkedSnapshot(eventId: Option[EventId] = None): Task[Checked[State]] =
    HttpClient.liftProblem(snapshot(eventId))

  def snapshot(eventId: Option[EventId] = None): Task[State]
