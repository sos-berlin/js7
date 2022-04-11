package js7.subagent.director

import js7.base.log.Logger
import js7.base.log.Logger.syntax._
import js7.base.monixutils.Switch
import js7.base.problem.Checked
import js7.base.stream.Numbered
import js7.data.subagent.{SubagentCommand, SubagentId, SubagentRunId}
import js7.subagent.director.SubagentDispatcher._
import monix.eval.Task

private final class SubagentDispatcher(
  subagentId: SubagentId,
  protected val postCommand: PostCommand)
extends CommandDispatcher
{
  protected type Command = SubagentCommand.Queueable

  protected def name = subagentId.toString

  // Required only for change of URI when it denotes the same running Subagent
  private def enqueueExecutes(previous: SubagentDispatcher): Task[Unit] =
    logger.debugTask(previous
      .queue
      .dequeueAll
      .flatMap(executes => queue
        .enqueueNumbered(executes
          .filter(_.value.command match {
            case _: SubagentCommand.OrderCommand => true
            case _: SubagentCommand.ReleaseEvents => false
            case _: SubagentCommand.ShutDown => false
          })
          .tapEach(cmd => logger.debug(s"enqueueExecutes $cmd"))
          .map(numbered => numbered.copy(
            value = new Execute(numbered.value.command, numbered.value.promise))))))

  override def toString = s"SubagentDispatcher($name)"
}

private object SubagentDispatcher
{
  type PostCommand = (Numbered[SubagentCommand.Queueable], SubagentRunId, Switch.ReadOnly) =>
    Task[Checked[Unit]]

  private val logger = Logger[this.type]
}
