package js7.agent

import akka.actor.{Actor, Props, Terminated}
import js7.agent.MainActor.*
import js7.agent.command.{CommandActor, CommandHandler}
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.scheduler.{AgentActor, AgentHandle}
import js7.base.auth.UserId
import js7.base.eventbus.StandardEventBus
import js7.base.log.{CorrelId, Logger}
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.{Allocated, ProgramTermination}
import js7.common.akkautils.CatchingSupervisorStrategy
import js7.core.command.CommandMeta
import js7.journal.recover.Recovered
import js7.journal.state.FileStatePersistence
import js7.launcher.configuration.JobLauncherConf
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Promise
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class MainActor(
  persistenceAllocated: Allocated[Task, FileStatePersistence[AgentState]],
  agentConfiguration: AgentConfiguration,
  testCommandHandler: Option[CommandHandler],
  readyPromise: Promise[Ready],
  terminationPromise: Promise[ProgramTermination],
  jobLauncherConf: JobLauncherConf,
  testEventBus: StandardEventBus[Any],
  clock: AlarmClock)
  (implicit scheduler: Scheduler, iox: IOExecutor)
extends Actor {

  import context.{actorOf, watch}

  override val supervisorStrategy = CatchingSupervisorStrategy(terminationPromise)

  private val agentActor = watch(actorOf(
    Props {
      new AgentActor(terminationPromise, persistenceAllocated, clock, agentConfiguration,
        jobLauncherConf, testEventBus)
    },
    "agent"))
  private val agentHandle = new AgentHandle(agentActor)

  private val commandHandler = testCommandHandler getOrElse {
    val actor = actorOf(Props {new CommandActor(agentHandle)}, "command")
    new CommandActor.Handle(actor)
  }

  private def api(meta: CommandMeta) = new DirectAgentApi(commandHandler, meta)

  override def preStart() = {
    super.preStart()
    for (t <- terminationPromise.future.failed) readyPromise.tryFailure(t)
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
    case MainActor.Input.Start(recovered) =>
      agentActor ! AgentActor.Input.Start(recovered)

    case AgentActor.Output.Ready =>
      readyPromise.success(Ready(api))

    case Input.ExternalCommand(cmd, userId, correlId, response) =>  // For RunningController
      correlId.bind {
        agentHandle.executeCommand(cmd, userId, response)
      }

    case Terminated(`agentActor`) =>
      logger.debug("Stop")
      terminationPromise.trySuccess(ProgramTermination())
      context.stop(self)
  }
}

object MainActor
{
  private val logger = Logger(getClass)

  final case class Ready(api: CommandMeta => DirectAgentApi)

  object Input {
    final case class Start(recovered: Recovered[AgentState])

    final case class ExternalCommand(
      command: AgentCommand,
      userId: UserId,
      correlId: CorrelId,
      response: Promise[Checked[AgentCommand.Response]])
  }
}
