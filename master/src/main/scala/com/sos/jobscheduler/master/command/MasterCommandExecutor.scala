package com.sos.jobscheduler.master.command

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.command.{CommandExecutor, CommandMeta, CommandRegister, CommandRun}
import com.sos.jobscheduler.core.startup.Halt
import com.sos.jobscheduler.data.command.{CommandHandlerDetailed, CommandHandlerOverview, InternalCommandId}
import com.sos.jobscheduler.master.command.MasterCommandExecutor._
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.MasterCommand.{Batch, EmergencyStop}
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
private[master] final class MasterCommandExecutor(otherCommandExecutor: CommandExecutor[MasterCommand])
extends CommandExecutor[MasterCommand]
{
  private val register = new CommandRegister[MasterCommand]

  def executeCommand(command: MasterCommand, meta: CommandMeta): Task[Checked[command.Response]] =
    executeCommand(command, meta, None)

  private def executeCommand(command: MasterCommand, meta: CommandMeta, batchId: Option[InternalCommandId]): Task[Checked[command.Response]] = {
    val run = register.add(meta.user.id, command, batchId)
    logCommand(run)
    executeCommand2(command, meta, run.internalId, batchId)
      .map { checkedResponse =>
        if (run.batchInternalId.isEmpty || checkedResponse != Right(MasterCommand.Response.Accepted)) {
          logger.debug(s"Response to ${run.idString} ${MasterCommand.jsonCodec.classToName(run.command.getClass)} (${run.runningSince.elapsed.pretty}): $checkedResponse")
        }
        checkedResponse.map(_.asInstanceOf[command.Response])
      }
      .doOnFinish(maybeThrowable => Task {
        for (t <- maybeThrowable if run.batchInternalId.isEmpty) {
          logger.warn(s"Command $run failed with ${t.toStringWithCauses}")
        }
        register.remove(run.internalId)
      })
  }

  private def executeCommand2(command: MasterCommand, meta: CommandMeta, id: InternalCommandId, batchId: Option[InternalCommandId])
  : Task[Checked[MasterCommand.Response]] =
    command match {
      case Batch(commands) =>
        val tasks = for (c <- commands) yield executeCommand(c, meta, batchId orElse Some(id))
        Task.sequence(tasks) map (checkedResponses => Right(Batch.Response(checkedResponses)))

      case EmergencyStop(restart) =>
        Halt.haltJava("Command EmergencyStop received: JOBSCHEDULER MASTER STOPS NOW", restart = restart)

      case _ =>
        otherCommandExecutor.executeCommand(command, meta)
    }

  private def logCommand(run: CommandRun[MasterCommand]): Unit =
    run.command match {
      case Batch(_) =>   // Log only individual commands
      case _ => logger.info(run.toString)
    }

  def overview: CommandHandlerOverview =
    register.overview

  def detailed: CommandHandlerDetailed[MasterCommand] =
    register.detailed
}

object MasterCommandExecutor {
  private val logger = Logger(getClass)
}
