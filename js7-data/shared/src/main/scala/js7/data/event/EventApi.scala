package js7.data.event

import io.circe.Decoder
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.data.cluster.ExtendedClusterState
import monix.eval.Task
import monix.reactive.Observable
import scala.collection.immutable.Seq
import scala.reflect.ClassTag

trait EventApi
extends SessionApi.HasUserAndPassword
with HasIsIgnorableStackTrace
{
  def extendedClusterState: Task[Checked[ExtendedClusterState]]

  def snapshot: Task[Checked[Stamped[Seq[Any]]]]

  def eventObservable[E <: Event: ClassTag](request: EventRequest[E])
    (implicit kd: Decoder[KeyedEvent[E]])
    : Task[Observable[Stamped[KeyedEvent[E]]]]
}
