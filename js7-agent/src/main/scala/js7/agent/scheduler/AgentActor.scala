package js7.agent.scheduler

import akka.actor.{Actor, ActorRef, Props, Stash, Terminated}
import akka.pattern.ask
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
import js7.cluster.ClusterNode
import js7.common.akkautils.{SimpleStateActor, SupervisorStrategies}
import js7.data.agent.Problems.{AgentAlreadyDedicatedProblem, AgentIsShuttingDown, AgentNotDedicatedProblem, AgentPathMismatchProblem, AgentRunIdMismatchProblem, AgentWrongControllerProblem}
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.state.FileStatePersistence
import js7.launcher.configuration.JobLauncherConf
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.Deadline
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private[agent] final class AgentActor(
  totalRunningSince: Deadline,
  terminatePromise: Promise[ProgramTermination],
  persistenceAllocated: Allocated[Task, FileStatePersistence[AgentState]],
  clusterNode: ClusterNode[AgentState],
  clock: AlarmClock,
  agentConf: AgentConfiguration,
  jobLauncherConf: JobLauncherConf,
  testEventBus: StandardEventBus[Any])
  (implicit protected val scheduler: Scheduler, iox: IOExecutor)
  extends Actor with Stash with SimpleStateActor
{
  import agentConf.{implicitAkkaAskTimeout, journalMeta}
  import context.{actorOf, watch}
  val persistence = persistenceAllocated.allocatedThing
  import persistence.eventWatch

  override val supervisorStrategy = SupervisorStrategies.escalate

  private var recoveredAgentState: AgentState = null
  private val started = SetOnce[Started]
  private val shutDownCommand = SetOnce[AgentCommand.ShutDown]
  private var isResetting = false
  private def terminating = shutDownCommand.isDefined
  private val terminateCompleted = Promise[Completed]()

  private val allocatedSignatureVerifier = DirectoryWatchingSignatureVerifier
    .checkedResource(
      config = agentConf.config,
      onUpdated = () => testEventBus.publish(ItemSignatureKeysUpdated))
    .orThrow
    .toAllocated
    .awaitInfinite

  override def preStart() = {
    watch(persistence.journalActor)
    super.preStart()
  }

  override def postStop() = {
    super.postStop()
    if (isResetting) {
      logger.warn("DELETE JOURNAL FILES DUE TO AGENT RESET")
      journalMeta.deleteJournal(ignoreFailure = true)
    }
    allocatedSignatureVerifier.stop.awaitInfinite
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

    case Terminated(actor) if actor == persistence.journalActor && terminating =>
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
        val agentRunId = AgentRunId(persistence.journalId)
        persistence
          .persist(agentState =>
            if (!agentState.isDedicated)
              for (_ <- UserId.checked(agentPath.string)/*used for Subagent login*/) yield
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
              .rightAs(eventAndState._2.eventId)
          })
          .runToFuture
          .onComplete { triedEventId =>
            response.complete(triedEventId.map(_.map(eventId =>
              AgentCommand.DedicateAgentDirector.Response(agentRunId, eventId))))
          }

      case AgentCommand.CoupleController(agentPath, agentRunId, eventId) if !terminating =>
        // Command does not change state. It only checks the coupling (for now)
        response.success(
          for {
            _ <- checkAgentPath(agentPath)
            _ <- checkAgentRunId(agentRunId)
            _ <- eventWatch.checkEventId(eventId)
          } yield
            CoupleController.Response(persistence.unsafeCurrentState().idToOrder.keySet))

      case AgentCommand.ClusterAppointNodes(idToUri, activeId) =>
        response.completeWith(
          Task(clusterNode.workingClusterNode)
            .flatMapT(_.appointNodes(idToUri, activeId))
            .rightAs(AgentCommand.Response.Accepted)
            .runToFuture)

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

  private def checkAgentPath(requestedAgentPath: AgentPath): Checked[Unit] = {
    val agentState = persistence.unsafeCurrentState()
    if (!agentState.isDedicated)
      Left(AgentNotDedicatedProblem)
    else if (requestedAgentPath != agentState.agentPath)
      Left(AgentPathMismatchProblem(requestedAgentPath, agentState.agentPath))
    else
      RightUnit
  }

  private def checkAgentRunId(requestedAgentRunId: AgentRunId): Checked[Unit] = {
    val agentState = persistence.unsafeCurrentState()
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
        //persistence.journalActor ! JournalActor.Input.Terminate
        persistenceAllocated.stop.runAsyncAndForget
      }
    }

  private def terminateOrderKeeper(shutDown: AgentCommand.ShutDown)
  : Future[Checked[AgentCommand.Response.Accepted]] = {
    shutDownCommand := shutDown
    val future = started.toOption match {
      case None =>
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
                totalRunningSince,
                requireNonNull(recoveredAgentState),
                allocatedSignatureVerifier.allocatedThing,
                jobLauncherConf,
                persistenceAllocated,
                clock,
                agentConf)
              },
            "AgentOrderKeeper")
          watch(actor)
          started := Started(agentPath, controllerId, actor)
          Checked.unit
      }
    }

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
