package js7.data.event

import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.problem.Checked
import js7.base.session.SessionApi
import io.circe.Decoder
import monix.eval.Task
import monix.reactive.Observable
import scala.collection.immutable.Seq
import scala.reflect.ClassTag

trait EventApi
extends SessionApi.HasUserAndPassword
with HasIsIgnorableStackTrace
{
  def snapshot: Task[Checked[Stamped[Seq[Any]]]]

  def eventObservable[E <: Event: ClassTag](request: EventRequest[E])
    (implicit kd: Decoder[KeyedEvent[E]])
    : Task[Observable[Stamped[KeyedEvent[E]]]]
}
