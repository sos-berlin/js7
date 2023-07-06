package js7.subagent

import io.circe.Decoder
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.stream.Numbered
import js7.core.command.CommandMeta
import js7.data.event.{Event, EventRequest, KeyedEvent, Stamped}
import js7.data.subagent.{SubagentCommand, SubagentRunId}
import js7.subagent.director.SubagentApi
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

private final class LocalSubagentApi(subagent: Subagent)
extends SubagentApi
with SessionApi.Dummy
{
  def isLocal = true

  override def hasRelevantStackTrace(throwable: Throwable): Boolean =
    true

  def eventObservable[E <: Event : ClassTag](
    request: EventRequest[E],
    subagentRunId: SubagentRunId,
    heartbeat: Option[FiniteDuration] = None)
    (implicit kd: Decoder[KeyedEvent[E]])
  : Task[Observable[Stamped[KeyedEvent[E]]]] =
    Task.pure(
      subagent.journal.eventWatch.observe(request))

  def executeSubagentCommand[A <: SubagentCommand](numbered: Numbered[A])
  : Task[Checked[numbered.value.Response]] =
    subagent.commandExecutor.executeCommand(numbered, CommandMeta.System)
      .asInstanceOf[Task[Checked[numbered.value.Response]]]
}