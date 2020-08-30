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
  type State <: JournaledState[State]

  def baseUri: Uri

  def clusterNodeState: Task[Checked[ClusterNodeState]]

  def eventObservable[E <: Event: ClassTag](request: EventRequest[E])(implicit kd: Decoder[KeyedEvent[E]])
  : Task[Observable[Stamped[KeyedEvent[E]]]]

  def snapshot(eventId: Option[EventId] = None): Task[Checked[State]]
}
