package js7.data.event

import io.circe.Decoder
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.web.Uri
import js7.data.cluster.ClusterNodeState
import monix.eval.Task
import monix.reactive.Observable
import scala.reflect.ClassTag

trait EventApi
extends SessionApi.HasUserAndPassword
with HasIsIgnorableStackTrace
{
  def baseUri: Uri

  def clusterNodeState: Task[Checked[ClusterNodeState]]

  def snapshot: Task[Checked[Observable[Any]]]

  def eventObservable[E <: Event: ClassTag](request: EventRequest[E])
    (implicit kd: Decoder[KeyedEvent[E]])
    : Task[Observable[Stamped[KeyedEvent[E]]]]

  final def snapshotAs[S <: JournaledState[S]](implicit S: JournaledState.Companion[S]): Task[Checked[S]] =
    snapshot.flatMap {
      case Left(problem) => Task.pure(Left(problem))
      case Right(o) => S.fromObservable(o) map Right.apply
    }
}
