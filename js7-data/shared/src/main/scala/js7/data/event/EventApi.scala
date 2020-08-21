package js7.data.event

import io.circe.Decoder
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.data.cluster.ClusterNodeState
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now
import scala.reflect.ClassTag

trait EventApi
extends SessionApi.HasUserAndPassword
with HasIsIgnorableStackTrace
{
  def baseUri: Uri

  def clusterNodeState: Task[Checked[ClusterNodeState]]

  def snapshot(eventId: Option[EventId]): Task[Checked[Observable[Any]]]

  def eventObservable[E <: Event: ClassTag](request: EventRequest[E])(implicit kd: Decoder[KeyedEvent[E]])
  : Task[Observable[Stamped[KeyedEvent[E]]]]

  final def snapshotAs[S <: JournaledState[S]](eventId: Option[EventId] = None)(implicit S: JournaledState.Companion[S])
  : Task[Checked[S]] =
    Task.defer {
      val startedAt = now
      snapshot(eventId)
        .logTiming(startedAt = startedAt, onComplete = (d, n, exitCase) =>
          scribe.debug(s"$S snapshot receive $exitCase - ${itemsPerSecondString(d, n, "objects")}"))
        .flatMapT(obs =>
          S.fromObservable(obs) map Right.apply)
    }
}
