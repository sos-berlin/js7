package js7.agent

import akka.actor.{Actor, Props, Terminated}
import js7.agent.MainActor.*
import js7.agent.command.{CommandActor, CommandHandler}
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.scheduler.{AgentActor, AgentHandle}
import js7.base.eventbus.StandardEventBus
import js7.base.log.Logger
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.{Allocated, ProgramTermination}
import js7.cluster.ClusterNode
import js7.common.akkautils.CatchingSupervisorStrategy
import js7.core.command.CommandMeta
import js7.data.subagent.SubagentId
import js7.journal.state.FileJournal
import js7.subagent.Subagent
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class MainActor(
  subagent: Subagent,
  totalRunningSince: Deadline,
  failedOverSubagentId: Option[SubagentId],
  journalAllocated: Allocated[Task, FileJournal[AgentState]],
  agentConfiguration: AgentConfiguration,
  testCommandHandler: Option[CommandHandler],
  readyPromise: Promise[Ready],
  terminationPromise: Promise[ProgramTermination],
  clusterNode: ClusterNode[AgentState],
  testEventBus: StandardEventBus[Any],
  clock: AlarmClock)
  (implicit scheduler: Scheduler, iox: IOExecutor)
extends Actor {

  import context.{actorOf, watch}

  override val supervisorStrategy = CatchingSupervisorStrategy(terminationPromise)

  private val agentActor = watch(actorOf(
    Props {
      new AgentActor(
        subagent,
        totalRunningSince, failedOverSubagentId,
        terminationPromise, journalAllocated,
        clusterNode,
        clock, agentConfiguration, testEventBus)
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
    final case class Start(agentState: AgentState)
  }
}
