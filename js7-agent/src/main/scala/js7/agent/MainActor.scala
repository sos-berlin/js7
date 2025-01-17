package js7.agent

import cats.effect.unsafe.IORuntime
import js7.agent.MainActor.*
import js7.agent.command.{CommandActor, CommandHandler}
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.scheduler.{AgentActor, AgentHandle}
import js7.base.log.Logger
import js7.base.time.AlarmClock
import js7.base.utils.SetOnce
import js7.cluster.WorkingClusterNode
import js7.common.pekkoutils.CatchingSupervisorStrategy
import js7.core.command.CommandMeta
import js7.data.subagent.SubagentId
import js7.subagent.Subagent
import org.apache.pekko.actor.{Actor, ActorRef, Props, SupervisorStrategy, Terminated}
import scala.concurrent.{ExecutionContext, Promise}
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class MainActor(
  forDirector: Subagent.ForDirector,
  failedOverSubagentId: Option[SubagentId],
  workingClusterNode: WorkingClusterNode[AgentState],
  agentConfiguration: AgentConfiguration,
  testCommandHandler: Option[CommandHandler],
  readyPromise: Promise[Ready],
  terminationPromise: Promise[DirectorTermination],
  clock: AlarmClock)
  (using ioRuntime: IORuntime)
extends Actor:

  import context.{actorOf, watch}
  private given ExecutionContext = ioRuntime.compute

  override val supervisorStrategy: SupervisorStrategy =
    CatchingSupervisorStrategy(terminationPromise)

  private val agentActor = watch(actorOf(
    Props {
      new AgentActor(
        forDirector,
        failedOverSubagentId,
        workingClusterNode,
        terminationPromise,
        clock, agentConfiguration)
    },
    "agent"))
  private val agentHandle = new AgentHandle(agentActor)

  val commandActor: SetOnce[ActorRef] = SetOnce[ActorRef]
  private val commandHandler = testCommandHandler getOrElse:
    // A global, not a child actor !!!
    // because a child Actor may stop before Shutdown command has been responded,
    // leaving the client without response (and tests fail after 99s)
    val actor = context.system.actorOf(Props {new CommandActor(agentHandle)}, "command")
    commandActor := actor
    new CommandActor.Handle(actor)

  private def api(meta: CommandMeta) = new DirectAgentApi(commandHandler, meta)

  override def preStart(): Unit =
    super.preStart()
    for t <- terminationPromise.future.failed do readyPromise.tryFailure(t)

  override def postStop(): Unit =
    if !readyPromise.isCompleted then
      readyPromise.tryFailure(new RuntimeException("MainActor has stopped before AgentActor has become ready") with NoStackTrace)
    if !terminationPromise.isCompleted then
      terminationPromise.tryFailure(new RuntimeException("MainActor has stopped unexpectedly") with NoStackTrace)
    logger.debug("Stopped")
    super.postStop()

  def receive: Receive =
    case MainActor.Input.Start(recovered) =>
      agentActor ! AgentActor.Input.Start(recovered)

    case AgentActor.Output.Ready =>
      readyPromise.success(Ready(api))

    case Terminated(`agentActor`) =>
      logger.debug("Stop")
      terminationPromise.trySuccess(DirectorTermination())
      context.stop(self)


object MainActor:
  private val logger = Logger[this.type]

  final case class Ready(api: CommandMeta => DirectAgentApi)

  object Input:
    final case class Start(agentState: AgentState)
