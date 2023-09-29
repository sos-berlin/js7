package js7.subagent.director

import io.circe.Decoder
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.stream.Numbered
import js7.data.event.{Event, EventRequest, KeyedEvent, Stamped}
import js7.data.subagent.{SubagentCommand, SubagentRunId}
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

trait SubagentApi extends SessionApi.HasUserAndPassword with HasIsIgnorableStackTrace:
  def eventObservable[E <: Event: ClassTag](
    request: EventRequest[E],
    subagentRunId: SubagentRunId,
    heartbeat: Option[FiniteDuration] = None)
    (implicit kd: Decoder[KeyedEvent[E]])
  : Task[Observable[Stamped[KeyedEvent[E]]]]

  def executeSubagentCommand[A <: SubagentCommand](numbered: Numbered[A])
  : Task[Checked[numbered.value.Response]]

  def isLocal: Boolean
