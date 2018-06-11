package com.sos.jobscheduler.master.command

import akka.actor.ActorRef
import akka.pattern.ask
import cats.data.Validated.Valid
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.akkahttp.web.session.{LoginSession, SessionRegister}
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.master.command.CommandExecutor._
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.MasterCommand.EmergencyStop
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
private[master] final class CommandExecutor(
  masterConfiguration: MasterConfiguration,
  sessionRegister: SessionRegister[LoginSession.Simple],
  orderKeeper: ActorRef)
{
  import masterConfiguration.akkaAskTimeout

  def executeCommand(command: MasterCommand, meta: CommandMeta): Task[Checked[command.MyResponse]] =
    executeCommand2(command, meta).map(_.map(_.asInstanceOf[command.MyResponse]))

  private def executeCommand2(command: MasterCommand, meta: CommandMeta): Task[Checked[MasterCommand.Response]] =
    command match {
      case EmergencyStop ⇒
        val msg = "Command EmergencyStop received: JOBSCHEDULER MASTER STOPS NOW"
        logger.error(msg)
        Log4j.shutdown()
        sys.runtime.halt(99)
        Task.pure(Valid(MasterCommand.Response.Accepted))  // unreachable

      case _ ⇒
        Task.deferFuture((orderKeeper ? command).mapTo[MasterCommand.Response]) map Valid.apply  // TODO MasterOrderKeeper should return Checked
    }
}

object CommandExecutor {
  private val logger = Logger(getClass)
}
