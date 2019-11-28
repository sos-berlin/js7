package com.sos.jobscheduler.agent

import akka.actor.{Actor, Props, Terminated}
import com.google.inject.Injector
import com.sos.jobscheduler.agent.MainActor._
import com.sos.jobscheduler.agent.command.{CommandActor, CommandHandler}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.AgentTermination
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.{AgentActor, AgentHandle}
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.akkautils.CatchingSupervisorStrategy
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.command.CommandMeta
import monix.execution.Scheduler
import scala.concurrent.Promise
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class MainActor(
  agentConfiguration: AgentConfiguration,
  sessionRegister: SessionRegister[SimpleSession],
  injector: Injector,
  readyPromise: Promise[Ready],
  terminationPromise: Promise[AgentTermination.Terminate])
extends Actor {

  import agentConfiguration.akkaAskTimeout
  import context.{actorOf, watch}

  override val supervisorStrategy = CatchingSupervisorStrategy(terminationPromise)

  private implicit val scheduler = injector.instance[Scheduler]
  private val agentActor = watch(actorOf(Props { injector.instance[AgentActor.Factory].apply(terminationPromise) }, "agent"))
  private val agentHandle = new AgentHandle(agentActor)(akkaAskTimeout)

  private val commandHandler = injector.option[CommandHandler] getOrElse { // Only tests bind a CommandHandler
    val actor = actorOf(Props { new CommandActor(agentHandle) }, "command")
    new CommandActor.Handle(actor)(akkaAskTimeout)
  }

  private def api(meta: CommandMeta) = new DirectAgentApi(commandHandler, agentHandle, meta)

  override def preStart() = {
    super.preStart()
    for (t <- terminationPromise.future.failed) readyPromise.tryFailure(t)
    agentActor ! AgentActor.Input.Start
  }

  override def postStop() = {
    if (!readyPromise.isCompleted) {
      readyPromise.tryFailure(new RuntimeException("MainActor has stopped before AgentActor has become ready") with NoStackTrace)
    }
    if (!terminationPromise.isCompleted) {
      terminationPromise.tryFailure(new RuntimeException("MainActor has stopped unexpectedly") with NoStackTrace)
    }
    logger.debug("Stopped")
    super.postStop()
  }

  def receive = {
    case AgentActor.Output.Ready =>
      readyPromise.success(Ready(api))

    case Input.ExternalCommand(userId, cmd, response) =>  // For RunningMaster
      agentHandle.executeCommand(cmd, userId, response)

    case Terminated(`agentActor`) =>
      logger.debug("Stop")
      terminationPromise.trySuccess(AgentTermination.Terminate())
      context.stop(self)
  }
}

object MainActor {
  private val logger = Logger(getClass)

  final case class Ready(api: CommandMeta => DirectAgentApi)

  object Input {
    final case class ExternalCommand(userId: UserId, command: AgentCommand, response: Promise[Checked[AgentCommand.Response]])
  }
}
