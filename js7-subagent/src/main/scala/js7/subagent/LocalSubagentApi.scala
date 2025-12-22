package js7.subagent

import cats.effect.IO
import fs2.Stream
import io.circe.Decoder
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.stream.Numbered
import js7.core.command.CommandMeta
import js7.data.event.{Event, EventRequest, KeyedEvent, Stamped}
import js7.data.subagent.{SubagentCommand, SubagentRunId}
import js7.subagent.director.SubagentApi
import scala.reflect.ClassTag

private final class LocalSubagentApi(subagent: Subagent)
extends SubagentApi, SessionApi.Dummy:

  def isLocal = true

  override def hasRelevantStackTrace(throwable: Throwable): Boolean =
    true

  def eventStream[E <: Event : ClassTag](
    request: EventRequest[E],
    subagentRunId: SubagentRunId)
    (using Decoder[KeyedEvent[E]])
  : IO[Stream[IO, Stamped[KeyedEvent[E]]]] =
    IO.pure:
      subagent.journal.eventWatch.stream(request)

  def executeSubagentCommand[A <: SubagentCommand](numbered: Numbered[A])
  : IO[Checked[numbered.value.Response]] =
    subagent.commandExecutor.executeCommand(
      numbered,
      CommandMeta.system("LocalSubagentApi")
    ).asInstanceOf[IO[Checked[numbered.value.Response]]]
