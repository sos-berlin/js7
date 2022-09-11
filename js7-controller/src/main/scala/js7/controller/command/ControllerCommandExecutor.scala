package js7.controller.command

import js7.base.log.{CorrelId, CorrelIdWrapped, Logger}
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.common.system.startup.Halt
import js7.controller.command.ControllerCommandExecutor._
import js7.core.command.{CommandExecutor, CommandMeta, CommandRegister, CommandRun}
import js7.data.command.{CommandHandlerDetailed, CommandHandlerOverview}
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{Batch, EmergencyStop}
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
private[controller] final class ControllerCommandExecutor(
  otherCommandExecutor: CommandExecutor[ControllerCommand])
extends CommandExecutor[ControllerCommand]
{
  private val register = new CommandRegister[ControllerCommand]

  def executeCommand(command: ControllerCommand, meta: CommandMeta): Task[Checked[command.Response]] =
    executeCommand(command, meta, None)

  private def executeCommand(command: ControllerCommand, meta: CommandMeta, batchId: Option[CorrelId])
  : Task[Checked[command.Response]] =
    Task.defer {
      val correlId = CorrelId.current
      val run = register.add(command, meta, correlId, batchId)
      logCommand(run)
      executeCommand2(command, meta, correlId, batchId)
        .map { checkedResponse =>
          if (run.batchInternalId.isEmpty) {
            checkedResponse match {
              //case Right(ControllerCommand.Response.Accepted) =>
              case Right(_) =>
                logger.debug(s"↙︎ ${run.idString} " +
                  ControllerCommand.jsonCodec.classToName(run.command.getClass) +
                  s" (${run.runningSince.elapsed.pretty}): $checkedResponse")
              case Left(problem) =>
            }
          }
          for (problem <- checkedResponse.left) logger.warn(s"$run rejected: $problem")
          checkedResponse.map(_.asInstanceOf[command.Response])
        }
        .doOnFinish(maybeThrowable => Task {
          for (t <- maybeThrowable if run.batchInternalId.isEmpty) {
            logger.warn(s"$run failed: ${t.toStringWithCauses}")
          }
          register.remove(run.correlId)
        })
    }

  private def executeCommand2(command: ControllerCommand, meta: CommandMeta, id: CorrelId, batchId: Option[CorrelId])
  : Task[Checked[ControllerCommand.Response]] =
    Task.defer {
      command match {
        case Batch(correlIdWrappedCommands) =>
          val tasks = for (CorrelIdWrapped(correlId, command) <- correlIdWrappedCommands) yield
            correlId.bind {
              executeCommand(command, meta, batchId orElse Some(id))
            }
          Task.sequence(tasks).map(checkedResponses => Right(Batch.Response(checkedResponses)))

        case EmergencyStop(restart) =>
          Halt.haltJava("Command EmergencyStop received: JS7 CONTROLLER STOPS NOW", restart = restart)

        case _ =>
          otherCommandExecutor.executeCommand(command, meta)
      }
    }

  private def logCommand(run: CommandRun[ControllerCommand]): Unit =
    run.command match {
      case Batch(_) =>   // Log only individual commands
      case _ => logger.debug(s"↘ $run")
    }

  def overview: CommandHandlerOverview =
    register.overview

  def detailed: CommandHandlerDetailed[ControllerCommand] =
    register.detailed
}

object ControllerCommandExecutor {
  private val logger = Logger(getClass)
}
