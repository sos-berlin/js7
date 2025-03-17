package js7.controller.command

import cats.effect.{IO, Outcome}
import cats.syntax.traverse.*
import js7.base.log.{CorrelId, CorrelIdWrapped, Logger}
import js7.base.problem.Checked
import js7.base.system.startup.Halt
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandExecutor.*
import js7.core.command.{CommandMeta, CommandRegister, CommandRun}
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{Batch, EmergencyStop}

trait IControllerCommandExecutor:
  def executeCommand(command: ControllerCommand, meta: CommandMeta): IO[Checked[command.Response]]


private[controller] final class ControllerCommandExecutor(
  otherCommandExecutor: IControllerCommandExecutor)
extends IControllerCommandExecutor:

  private val register = new CommandRegister[ControllerCommand]

  def executeCommand(command: ControllerCommand, meta: CommandMeta): IO[Checked[command.Response]] =
    executeCommand(command, meta, None)

  private def executeCommand(command: ControllerCommand, meta: CommandMeta, batchId: Option[CorrelId])
  : IO[Checked[command.Response]] =
    IO.defer:
      val correlId = CorrelId.current
      val run = register.add(command, meta, correlId, batchId)
      logCommand(run)
      executeCommand2(command, meta, correlId, batchId)
        .map { checkedResponse =>
          if run.batchInternalId.isEmpty then
            checkedResponse match
              //case Right(ControllerCommand.Response.Accepted) =>
              case Right(_) =>
                logger.debug(s"↙ ${run.idString} " +
                  ControllerCommand.jsonCodec.classToName(run.command.getClass) +
                  s" (${run.runningSince.elapsed.pretty}): $checkedResponse")
              case Left(problem) =>
          for problem <- checkedResponse.left do logger.warn(s"$run rejected: $problem")
          checkedResponse.map(_.asInstanceOf[command.Response])
        }
        .guaranteeCase:
          case Outcome.Errored(t) if run.batchInternalId.isEmpty =>
            IO(logger.warn(s"$run failed: ${t.toStringWithCauses}"))
          case _ => IO.unit
        .guarantee:
          IO(register.remove(run.correlId))

  private def executeCommand2(command: ControllerCommand, meta: CommandMeta, id: CorrelId, batchId: Option[CorrelId])
  : IO[Checked[ControllerCommand.Response]] =
    IO.defer:
      command match
        case Batch(correlIdWrappedCommands) =>
          val ios = for CorrelIdWrapped(correlId, command) <- correlIdWrappedCommands yield
            correlId.bind:
              executeCommand(command, meta, batchId orElse Some(id))
          ios.sequence.map(checkedResponses => Right(Batch.Response(checkedResponses)))

        case EmergencyStop(restart) =>
          Halt.haltJava("🟥 EmergencyStop command received: JS7 CONTROLLER STOPS NOW", restart = restart)

        case _ =>
          otherCommandExecutor.executeCommand(command, meta)

  private def logCommand(run: CommandRun[ControllerCommand]): Unit =
    run.command match
      case Batch(_) =>   // Log only individual commands
      case _ => logger.debug(s"↘ $run")


object ControllerCommandExecutor:
  private val logger = Logger[this.type]
