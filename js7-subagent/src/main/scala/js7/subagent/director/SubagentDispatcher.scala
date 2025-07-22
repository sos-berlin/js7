package js7.subagent.director

import cats.effect.IO
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.Switch
import js7.base.problem.Checked
import js7.base.stream.Numbered
import js7.data.subagent.{SubagentCommand, SubagentId, SubagentRunId}
import js7.subagent.director.SubagentDispatcher.*

private final class SubagentDispatcher(
  subagentId: SubagentId,
  protected val postCommand: PostCommand)
extends CommandDispatcher:
  protected type Command = SubagentCommand.Queueable
  protected type Response = Unit

  protected def name = subagentId.toString

  // Required only for change of URI when it denotes the same running Subagent
  private def enqueueExecutes(previous: SubagentDispatcher): IO[Unit] =
    logger.debugIO:
      previous.queue.stop
        .flatMap: executes =>
          queue.enqueueNumbered:
            executes.filter:
              _.value.command match
                case _: SubagentCommand.OrderCommand => true
                case _: SubagentCommand.DetachProcessedOrders => true
                case _: SubagentCommand.ReleaseEvents => false
                case _: SubagentCommand.ShutDown => false
            .tapEach(cmd => logger.debug(s"enqueueExecutes $cmd"))
            .map: numbered =>
              numbered.copy(
                value = new Execute(numbered.value.command, numbered.value.promise))

  override def toString = s"SubagentDispatcher($name)"

private object SubagentDispatcher:
  type PostCommand = (Numbered[SubagentCommand.Queueable], SubagentRunId, Switch.ReadOnly) =>
    IO[Checked[Unit]]

  private val logger = Logger[this.type]
