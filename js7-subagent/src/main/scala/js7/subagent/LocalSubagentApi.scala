package js7.subagent

import io.circe.Decoder
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.stream.Numbered
import js7.data.event.{Event, EventRequest, KeyedEvent, Stamped}
import js7.data.subagent.{SubagentCommand, SubagentRunId}
import js7.subagent.director.SubagentApi
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

final class LocalSubagentApi(subagent: Subagent)
extends SubagentApi
with SessionApi.Dummy
{
  def isLocal = true

  override def isIgnorableStackTrace(throwable: Throwable): Boolean =
    false

  def eventObservable[E <: Event : ClassTag](
    request: EventRequest[E],
    subagentRunId: SubagentRunId,
    heartbeat: Option[FiniteDuration] = None)
    (implicit kd: Decoder[KeyedEvent[E]])
  : Task[Observable[Stamped[KeyedEvent[E]]]] =
    Task {
      subagent.journal.eventWatch.observe(request)
    }

  def executeSubagentCommand[A <: SubagentCommand](numbered: Numbered[A])
  : Task[Checked[numbered.value.Response]] =
    subagent.commandExecutor.executeCommand(numbered)
      .asInstanceOf[Task[Checked[numbered.value.Response]]]
}
