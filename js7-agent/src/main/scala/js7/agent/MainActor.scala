package js7.agent

import akka.actor.{Actor, Props, Terminated}
import com.google.inject.Injector
import js7.agent.MainActor._
import js7.agent.command.{CommandActor, CommandHandler}
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentTermination
import js7.agent.data.commands.AgentCommand
import js7.agent.scheduler.{AgentActor, AgentHandle}
import js7.base.auth.UserId
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.common.akkautils.CatchingSupervisorStrategy
import js7.common.guice.GuiceImplicits.RichInjector
import js7.core.command.CommandMeta
import monix.execution.Scheduler
import scala.concurrent.Promise
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class MainActor(
  agentConfiguration: AgentConfiguration,
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

    case Input.ExternalCommand(userId, cmd, response) =>  // For RunningController
      agentHandle.executeCommand(cmd, userId, response)

    case Terminated(`agentActor`) =>
      logger.debug("Stop")
      terminationPromise.trySuccess(AgentTermination.Terminate())
      context.stop(self)
  }
}

object MainActor
{
  private val logger = Logger(getClass)

  final case class Ready(api: CommandMeta => DirectAgentApi)

  object Input {
    final case class ExternalCommand(userId: UserId, command: AgentCommand, response: Promise[Checked[AgentCommand.Response]])
  }
}
