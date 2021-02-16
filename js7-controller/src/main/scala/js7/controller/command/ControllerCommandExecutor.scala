package js7.controller.command

import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.common.system.startup.Halt
import js7.controller.command.ControllerCommandExecutor._
import js7.core.command.{CommandExecutor, CommandMeta, CommandRegister, CommandRun}
import js7.data.command.{CommandHandlerDetailed, CommandHandlerOverview, InternalCommandId}
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{Batch, EmergencyStop}
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
private[controller] final class ControllerCommandExecutor(otherCommandExecutor: CommandExecutor[ControllerCommand])
extends CommandExecutor[ControllerCommand]
{
  private val register = new CommandRegister[ControllerCommand]

  def executeCommand(command: ControllerCommand, meta: CommandMeta): Task[Checked[command.Response]] =
    executeCommand(command, meta, None)

  private def executeCommand(command: ControllerCommand, meta: CommandMeta, batchId: Option[InternalCommandId])
  : Task[Checked[command.Response]] = {
    val run = register.add(meta.user.id, command, batchId)
    logCommand(run)
    executeCommand2(command, meta, run.internalId, batchId)
      .map { checkedResponse =>
        if (run.batchInternalId.isEmpty || checkedResponse != Right(ControllerCommand.Response.Accepted)) {
          logger.debug(s"Response to ${run.idString} ${ControllerCommand.jsonCodec.classToName(run.command.getClass)}" +
            s" (${run.runningSince.elapsed.pretty}): $checkedResponse")
        }
        for (problem <- checkedResponse.left) logger.warn(s"$run rejected: $problem")
        checkedResponse.map(_.asInstanceOf[command.Response])
      }
      .doOnFinish(maybeThrowable => Task {
        for (t <- maybeThrowable if run.batchInternalId.isEmpty) {
          logger.warn(s"$run failed: ${t.toStringWithCauses}")
        }
        register.remove(run.internalId)
      })
  }

  private def executeCommand2(command: ControllerCommand, meta: CommandMeta, id: InternalCommandId, batchId: Option[InternalCommandId])
  : Task[Checked[ControllerCommand.Response]] =
    command match {
      case Batch(commands) =>
        val tasks = for (c <- commands) yield executeCommand(c, meta, batchId orElse Some(id))
        Task.sequence(tasks).map(checkedResponses => Right(Batch.Response(checkedResponses)))

      case EmergencyStop(restart) =>
        Halt.haltJava("Command EmergencyStop received: JS7 CONTROLLER STOPS NOW", restart = restart)

      case _ =>
        otherCommandExecutor.executeCommand(command, meta)
    }

  private def logCommand(run: CommandRun[ControllerCommand]): Unit =
    run.command match {
      case Batch(_) =>   // Log only individual commands
      case _ => logger.info(run.toString)
    }

  def overview: CommandHandlerOverview =
    register.overview

  def detailed: CommandHandlerDetailed[ControllerCommand] =
    register.detailed
}

object ControllerCommandExecutor {
  private val logger = Logger(getClass)
}
