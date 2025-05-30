package js7.subagent.director

import cats.effect.IO
import fs2.Stream
import io.circe.Decoder
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.stream.Numbered
import js7.data.event.{Event, EventRequest, KeyedEvent, Stamped}
import js7.data.subagent.{SubagentCommand, SubagentRunId}
import scala.reflect.ClassTag

trait SubagentApi extends SessionApi.HasUserAndPassword, HasIsIgnorableStackTrace:

  def eventStream[E <: Event: ClassTag](
    request: EventRequest[E],
    subagentRunId: SubagentRunId)
    (using Decoder[KeyedEvent[E]])
  : IO[Stream[IO, Stamped[KeyedEvent[E]]]]

  def executeSubagentCommand[A <: SubagentCommand](numbered: Numbered[A])
  : IO[Checked[numbered.value.Response]]

  def isLocal: Boolean
