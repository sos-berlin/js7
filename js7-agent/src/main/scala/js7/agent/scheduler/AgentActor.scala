package js7.agent.scheduler

import akka.actor.{Actor, ActorRef, Props, Stash, Terminated}
import akka.pattern.ask
import java.util.Objects.requireNonNull
import javax.inject.{Inject, Singleton}
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.CoupleController
import js7.agent.data.event.AgentEvent.AgentDedicated
import js7.agent.data.views.AgentOverview
import js7.agent.scheduler.AgentActor._
import js7.agent.scheduler.order.AgentOrderKeeper
import js7.base.BuildInfo
import js7.base.auth.UserId
import js7.base.generic.Completed
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.CorrelIdBinder.{bindCorrelId, currentCorrelId}
import js7.base.log.{CorrelId, Logger}
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.ScalaUtils.RightUnit
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{ProgramTermination, SetOnce}
import js7.common.akkautils.{SimpleStateActor, SupervisorStrategies}
import js7.common.crypt.generic.GenericSignatureVerifier
import js7.common.system.JavaInformations.javaInformation
import js7.common.system.SystemInformations.systemInformation
import js7.common.system.startup.StartUp
import js7.data.agent.Problems.{AgentAlreadyDedicatedProblem, AgentIsShuttingDown, AgentNotDedicatedProblem, AgentPathMismatchProblem, AgentRunIdMismatchProblem, AgentWrongControllerProblem}
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.recover.Recovered
import js7.journal.state.FileStatePersistence
import js7.launcher.configuration.JobLauncherConf
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private[agent] final class AgentActor private(
  terminatePromise: Promise[ProgramTermination],
  persistence: FileStatePersistence[AgentState],
  clock: AlarmClock,
  agentConf: AgentConfiguration,
  jobLauncherConf: JobLauncherConf)
  (implicit protected val scheduler: Scheduler, iox: IOExecutor)

  extends Actor with Stash with SimpleStateActor
{
  import agentConf.{akkaAskTimeout, journalMeta}
  import context.{actorOf, watch}
  import persistence.eventWatch

  override val supervisorStrategy = SupervisorStrategies.escalate

  private val signatureVerifier = GenericSignatureVerifier(agentConf.config).orThrow
  private var recovered: Recovered[AgentState] = null
  private val started = SetOnce[Started]
  private val shutDownCommand = SetOnce[AgentCommand.ShutDown]
  private var isResetting = false
  private def terminating = shutDownCommand.isDefined
  private val terminateCompleted = Promise[Completed]()

  override def preStart() = {
    watch(persistence.journalActor)
    super.preStart()
  }

  override def postStop() = {
    super.postStop()
    terminatePromise.trySuccess(
      ProgramTermination(restart = shutDownCommand.toOption.fold(false)(_.restart)))
    if (isResetting) {
      logger.warn("DELETE JOURNAL FILES DUE TO AGENT RESET")
      journalMeta.deleteJournal(ignoreFailure = true)
    }
    logger.debug("Stopped")
  }

  def receive = {
    case Input.Start(recovered) =>
      this.recovered = recovered
      val state = recovered.state
      if (state.isDedicated) {
        addOrderKeeper(state.agentPath, state.controllerId).orThrow
      }
      become("ready")(ready)
      sender() ! Output.Ready
  }

  private def ready: Receive = {
    case cmd: Input.ExternalCommand =>
      bindCorrelId(cmd.correlId) {
        executeExternalCommand(cmd)
      }

    case Terminated(a) if started.toOption.exists(_.actor == a) =>
      logger.debug("AgentOrderKeeper terminated")
      continueTermination()

    case Terminated(actor) if actor == persistence.journalActor && terminating =>
      for (_ <- terminateCompleted.future) {
        context.stop(self)
      }

    case Command.GetOverview =>
      sender() ! AgentOverview(
        version = BuildInfo.prettyVersion,
        buildId = BuildInfo.buildId,
        startedAt = StartUp.startedAt,
        isTerminating = terminating,
        system = systemInformation(),
        java = javaInformation)
  }

  private def executeExternalCommand(externalCommand: Input.ExternalCommand): Unit = {
    import externalCommand.{command, response}
    command match {
      case command: AgentCommand.ShutDown =>
        if (!terminating) {
          logger.info(s"❗️ $command")
          response.completeWith(terminateOrderKeeper(command))
        }

      case AgentCommand.Reset(maybeAgentRunId) =>
        logger.info(s"❗️ $command")
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

      case AgentCommand.DedicateAgentDirector(maybeSubagentId, controllerId, agentPath)
        if !terminating =>
        // Command is idempotent until AgentState has been touched
        val agentRunId = AgentRunId(persistence.journalId)
        persistence
          .persist(agentState =>
            if (!agentState.isDedicated)
              for (_ <- UserId.checked(agentPath.string)/*used for Subagent login*/) yield
                Seq(NoKey <-:
                  AgentDedicated(maybeSubagentId, agentPath, agentRunId, controllerId))
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
            CoupleController.Response(persistence.currentState.idToOrder.keySet))

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
              AgentOrderKeeper.Input.ExternalCommand(command, currentCorrelId, response))
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
    val agentState = persistence.currentState
    if (!agentState.isDedicated)
      Left(AgentNotDedicatedProblem)
    else if (requestedAgentPath != agentState.agentPath)
      Left(AgentPathMismatchProblem(requestedAgentPath, agentState.agentPath))
    else
      RightUnit
  }

  private def checkAgentRunId(requestedAgentRunId: AgentRunId): Checked[Unit] = {
    val agentState = persistence.currentState
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
        // When no AgentOrderKeeper has been startet, we need to stop the journal ourselves
        //persistence.journalActor ! JournalActor.Input.Terminate
        persistence.stop.runAsyncAndForget
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
          val recovered = this.recovered
          this.recovered = null  // release memory
          val actor = actorOf(
            Props {
              new AgentOrderKeeper(
                recovered.totalRunningSince,
                requireNonNull(recovered),
                signatureVerifier,
                jobLauncherConf,
                persistence,
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

  object Command {
    case object GetOverview
  }

  object Input {
    final case class Start(recovered: Recovered[AgentState])
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

  @Singleton
  final class Factory @Inject private(
    clock: AlarmClock,
    agentConfiguration: AgentConfiguration,
    jobLauncherConf: JobLauncherConf)
    (implicit scheduler: Scheduler, iox: IOExecutor)
  {
    def apply(
      persistence: FileStatePersistence[AgentState],
      terminatePromise: Promise[ProgramTermination])
    =
      new AgentActor(terminatePromise, persistence,
        clock, agentConfiguration, jobLauncherConf)
  }
}
