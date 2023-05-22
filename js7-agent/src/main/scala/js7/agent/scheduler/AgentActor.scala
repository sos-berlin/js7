package js7.agent.scheduler

import akka.actor.{Actor, ActorRef, Props, Stash, Terminated}
import akka.pattern.ask
import cats.syntax.traverse.*
import java.util.Objects.requireNonNull
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.CoupleController
import js7.agent.data.event.AgentEvent.AgentDedicated
import js7.agent.scheduler.AgentActor.*
import js7.agent.scheduler.order.AgentOrderKeeper
import js7.base.auth.UserId
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier
import js7.base.eventbus.StandardEventBus
import js7.base.generic.Completed
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.thread.IOExecutor
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.AlarmClock
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.RightUnit
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ProgramTermination, SetOnce}
import js7.base.web.Uri
import js7.cluster.ClusterNode
import js7.common.akkautils.{SimpleStateActor, SupervisorStrategies}
import js7.data.agent.Problems.{AgentAlreadyDedicatedProblem, AgentIsShuttingDown, AgentNotDedicatedProblem, AgentPathMismatchProblem, AgentRunIdMismatchProblem, AgentWrongControllerProblem}
import js7.data.agent.{AgentClusterConf, AgentPath, AgentRef, AgentRunId}
import js7.data.cluster.ClusterState
import js7.data.cluster.ClusterState.HasNodes
import js7.data.controller.ControllerId
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.node.NodeId
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.state.FileJournal
import js7.subagent.Subagent
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.Deadline
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private[agent] final class AgentActor(
  subagent: Subagent,
  totalRunningSince: Deadline,
  failedOverSubagentId: Option[SubagentId],
  terminatePromise: Promise[ProgramTermination],
  journalAllocated: Allocated[Task, FileJournal[AgentState]],
  clusterNode: ClusterNode[AgentState],
  clock: AlarmClock,
  agentConf: AgentConfiguration,
  testEventBus: StandardEventBus[Any])
  (implicit protected val scheduler: Scheduler, iox: IOExecutor)
  extends Actor with Stash with SimpleStateActor
{
  import agentConf.{implicitAkkaAskTimeout, journalMeta}
  import context.{actorOf, watch}
  val journal = journalAllocated.allocatedThing
  import journal.eventWatch

  override val supervisorStrategy = SupervisorStrategies.escalate

  private var recoveredAgentState: AgentState = null
  private val started = SetOnce[Started]
  private val shutDownCommand = SetOnce[AgentCommand.ShutDown]
  private var isResetting = false
  private def terminating = shutDownCommand.isDefined
  private val terminateCompleted = Promise[Completed]()

  // TODO Use Subagent's DirectoryWatchingSignatureVerifier
  private val allocatedSignatureVerifier = DirectoryWatchingSignatureVerifier
    .checkedResource(
      config = agentConf.config,
      onUpdated = () => testEventBus.publish(ItemSignatureKeysUpdated))
    .orThrow
    .toAllocated
    .awaitInfinite

  override def preStart() = {
    watch(journal.journalActor)
    super.preStart()
  }

  override def postStop() = {
    super.postStop()
    if (isResetting) {
      logger.warn("DELETE JOURNAL FILES DUE TO AGENT RESET")
      journalMeta.deleteJournal(ignoreFailure = true)
    }
    allocatedSignatureVerifier.release.awaitInfinite
    terminatePromise.trySuccess(
      ProgramTermination(restart = shutDownCommand.toOption.fold(false)(_.restart)))
    logger.debug("Stopped")
  }

  def receive = {
    case Input.Start(recoveredAgentState) =>
      this.recoveredAgentState = recoveredAgentState
      if (recoveredAgentState.isDedicated) {
        addOrderKeeper(recoveredAgentState.agentPath, recoveredAgentState.controllerId).orThrow
      }
      become("ready")(ready)
      sender() ! Output.Ready
  }

  private def ready: Receive = {
    case cmd: Input.ExternalCommand =>
      cmd.correlId.bind {
        executeExternalCommand(cmd)
      }

    case Terminated(a) if started.toOption.exists(_.actor == a) =>
      logger.debug("AgentOrderKeeper terminated")
      continueTermination()

    case Terminated(actor) if actor == journal.journalActor && terminating =>
      for (_ <- terminateCompleted.future) {
        context.stop(self)
      }
  }

  private def executeExternalCommand(externalCommand: Input.ExternalCommand): Unit = {
    import externalCommand.{command, response}
    command match {
      case command: AgentCommand.ShutDown =>
        if (!terminating) {
          response.completeWith(terminateOrderKeeper(command))
        }

      case AgentCommand.Reset(maybeAgentRunId) =>
        logger.info(s"❗ $command")
        maybeAgentRunId.fold(Checked.unit)(checkAgentRunId(_)) match {
          case Left(problem) => response.success(Left(problem))
          case Right(()) =>
            isResetting = true
            if (!terminating) {
              response.completeWith(terminateOrderKeeper(
                AgentCommand.ShutDown(processSignal = Some(SIGKILL),
                  suppressSnapshot = true, restart = true)))
            }
        }

      case AgentCommand.DedicateAgentDirector(directors, controllerId, agentPath)
        if !terminating =>
        // Command is idempotent until AgentState has been touched
        dedicate(directors, controllerId, agentPath)
          .runToFuture
          .onComplete { tried =>
            response.complete(
              tried.map(_.map { case (agentRunId, eventId) =>
                AgentCommand.DedicateAgentDirector.Response(agentRunId, eventId)
              }))
          }

      case AgentCommand.CoupleController(agentPath, agentRunId, eventId) if !terminating =>
        // Command does not change state. It only checks the coupling (for now)
        response.success(
          for {
            _ <- checkAgentPath(agentPath)
            _ <- checkAgentRunId(agentRunId)
            _ <- eventWatch.checkEventId(eventId)
          } yield
            CoupleController.Response(journal.unsafeCurrentState().idToOrder.keySet))

      case AgentCommand.ClusterSwitchOver =>
        response.completeWith(
          Task.left(Problem("Agent still not support ClusterSwitchOver command"))
          // Notify AgentOrderKeeper ???
          //Task(clusterNode.workingClusterNode)
          //  .flatMapT(_.switchOver)
          //  .rightAs(AgentCommand.Response.Accepted)
            .runToFuture)

      case command @ (_: AgentCommand.OrderCommand |
                      _: AgentCommand.TakeSnapshot |
                      _: AgentCommand.AttachItem |
                      _: AgentCommand.AttachSignedItem |
                      _: AgentCommand.DetachItem |
                      _: AgentCommand.ResetSubagent) =>
        // TODO Check AgentRunId ?
        started.toOption match {
          case None =>
            response.success(Left(AgentNotDedicatedProblem))
          case Some(started) =>
            started.actor.forward(
              AgentOrderKeeper.Input.ExternalCommand(command, CorrelId.current, response))
        }

      case command =>
        response.failure(
          if (terminating)
            AgentIsShuttingDown.throwable
          else
            new RuntimeException(s"Unexpected command for AgentActor: $command"))
    }
  }

  private def dedicate(directors: Seq[SubagentId], controllerId: ControllerId, agentPath: AgentPath)
  : Task[Checked[(AgentRunId, EventId)]] =
    Task.defer {
      // Command is idempotent until AgentState has been touched
      val agentRunId = AgentRunId(journal.journalId)
      journal
        .persist(agentState =>
          if (!agentState.isDedicated
            || agentPath == agentState.agentPath
            && agentState.meta.directors != directors)
            for (_ <- UserId.checked(agentPath.string) /*used for Subagent login*/ ) yield
              Seq(NoKey <-:
                AgentDedicated(directors, agentPath, agentRunId, controllerId))
          else if (agentPath != agentState.agentPath)
            Left(AgentPathMismatchProblem(agentPath, agentState.agentPath))
          else if (controllerId != agentState.meta.controllerId)
            Left(AgentWrongControllerProblem(controllerId, agentState.meta.controllerId))
          else if (!agentState.isFreshlyDedicated)
            Left(AgentAlreadyDedicatedProblem)
          else
            Right(Nil))
        .flatMapT(eventAndState => Task {
          logger.info(s"Dedicating $agentPath to '$controllerId'")
          addOrderKeeper(agentPath, controllerId)
            .rightAs(agentRunId -> eventAndState._2.eventId)
        })
    }

  private def checkAgentPath(requestedAgentPath: AgentPath): Checked[Unit] = {
    val agentState = journal.unsafeCurrentState()
    if (!agentState.isDedicated)
      Left(AgentNotDedicatedProblem)
    else if (requestedAgentPath != agentState.agentPath)
      Left(AgentPathMismatchProblem(requestedAgentPath, agentState.agentPath))
    else
      RightUnit
  }

  private def checkAgentRunId(requestedAgentRunId: AgentRunId): Checked[Unit] = {
    val agentState = journal.unsafeCurrentState()
    if (!agentState.isDedicated)
      Left(AgentNotDedicatedProblem)
    else if (requestedAgentRunId != agentState.meta.agentRunId) {
      val problem = AgentRunIdMismatchProblem(agentState.meta.agentPath)
      logger.warn(
        s"$problem, requestedAgentRunId=$requestedAgentRunId, agentRunId=${agentState.meta.agentRunId}")
      Left(problem)
    } else
      Checked.unit
  }

  private def continueTermination(): Unit =
    if (terminating) {
      if (started.isEmpty) {
        // When no AgentOrderKeeper has been started, we need to stop the journal ourselve
        //journal.journalActor ! JournalActor.Input.Terminate
        journalAllocated.release.runAsyncAndForget
      }
    }

  private def terminateOrderKeeper(shutDown: AgentCommand.ShutDown)
  : Future[Checked[AgentCommand.Response.Accepted]] = {
    shutDownCommand := shutDown
    val future = started.toOption match {
      case None =>
        logger.debug("⚠️ terminateOrderKeeper but AgentOrderKeeper has not been started")
        Future.successful(AgentCommand.Response.Accepted)
      case Some(started) =>
        (started.actor ? shutDown).mapTo[AgentCommand.Response.Accepted]
    }
    future.map { ordersTerminated =>
      terminateCompleted.success(Completed)  // Wait for child Actor termination
      continueTermination()
      Right(ordersTerminated)
    }
  }

  private def addOrderKeeper(agentPath: AgentPath, controllerId: ControllerId): Checked[Unit] =
    synchronized {
      started.toOption match {
        case Some(started) =>
          Left(Problem(
            s"This Agent has already started as '${started.agentPath}' for '${started.controllerId}'"))

        case None =>
          val recoveredAgentState = this.recoveredAgentState
          this.recoveredAgentState = null  // release memory
          val actor = actorOf(
            Props {
              new AgentOrderKeeper(
                subagent,
                totalRunningSince,
                failedOverSubagentId,
                requireNonNull(recoveredAgentState),
                appointClusterNodes,
                allocatedSignatureVerifier.allocatedThing,
                journalAllocated,
                clock,
                agentConf)
              },
            "AgentOrderKeeper")
          watch(actor)
          started := Started(agentPath, controllerId, actor)
          Checked.unit
      }
    }

  private def appointClusterNodes: Task[Unit] =
    logger.debugTask(journal.state
      .flatMap(agentState =>
        Task.when(agentState.isDedicated)(
          demandedClusterNodeUris(agentState)
            .fold(Task.unit) { idToUri =>
              agentState.clusterState
                .match_ {
                  case ClusterState.Empty =>
                    Some(AgentClusterConf.primaryNodeId)
                  case clusterState: HasNodes =>
                    (clusterState.setting.idToUri != idToUri) ? clusterState.activeId
                }
                .fold(Task.unit)(activeNodeId =>
                  Task(clusterNode.workingClusterNode)
                    .flatMapT(_.appointNodes(idToUri, activeNodeId))
                    .map(_.onProblemHandle(problem =>
                      logger.error(s"appointClusterNodes: $problem"))))
            })))

  /** Returns Some when AgentRef and the Director's SubagentIds are available. */
  private def demandedClusterNodeUris(state: AgentState): Option[Map[NodeId, Uri]] =
    for {
      agentRef <- state.keyToItem(AgentRef).get(state.meta.agentPath)
      if agentRef.directors.length == 2
      subagentItems <- agentRef.directors.traverse(state.keyToItem(SubagentItem).get)
      if subagentItems.length == 2
    } yield
      Map(
        AgentClusterConf.primaryNodeId -> subagentItems(0).uri,
        AgentClusterConf.backupNodeId -> subagentItems(1).uri)


  override def toString = "AgentActor"
}

object AgentActor
{
  private val logger = Logger(getClass)

  object Input {
    final case class Start(agentState: AgentState)
    final case class ExternalCommand(
      userId: UserId,
      command: AgentCommand,
      correlId: CorrelId,
      response: Promise[Checked[AgentCommand.Response]])
  }

  object Output {
    case object Ready
  }

  private final case class Started(
    agentPath: AgentPath,
    controllerId: ControllerId,
    actor: ActorRef)

  type ItemSignatureKeysUpdated = ItemSignatureKeysUpdated.type
  case object ItemSignatureKeysUpdated
}
