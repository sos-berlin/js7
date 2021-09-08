package js7.agent.scheduler

import akka.actor.{Actor, ActorRef, Props, Stash, Terminated}
import akka.pattern.{ask, pipe}
import com.softwaremill.diffx.generic.auto._
import java.util.Objects.requireNonNull
import javax.inject.{Inject, Singleton}
import js7.agent.configuration.{AgentConfiguration, AgentStartInformation}
import js7.agent.data.Problems.{AgentAlreadyCreatedProblem, AgentIsShuttingDown, AgentNotCreatedProblem, AgentPathMismatchProblem, AgentWrongControllerProblem}
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.CoupleController
import js7.agent.data.event.AgentEvent.AgentCreated
import js7.agent.data.views.AgentOverview
import js7.agent.data.{AgentState, AgentTermination}
import js7.agent.scheduler.AgentActor._
import js7.agent.scheduler.order.AgentOrderKeeper
import js7.base.auth.UserId
import js7.base.generic.Completed
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.ScalaUtils.RightUnit
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{Closer, SetOnce}
import js7.common.akkautils.{SimpleStateActor, SupervisorStrategies}
import js7.common.crypt.generic.GenericSignatureVerifier
import js7.common.system.JavaInformations.javaInformation
import js7.common.system.SystemInformations.systemInformation
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.executor.configuration.JobExecutorConf
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.recover.{JournaledStateRecoverer, Recovered}
import js7.journal.state.JournaledStatePersistence
import js7.journal.watch.JournalEventWatch
import js7.journal.{EventIdGenerator, StampedKeyedEventBus}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
private[agent] final class AgentActor private(
  terminatePromise: Promise[AgentTermination.Terminate],
  clock: AlarmClock,
  agentConf: AgentConfiguration,
  executorConf: JobExecutorConf,
  eventIdGenerator: EventIdGenerator,
  keyedEventBus: StampedKeyedEventBus)
  (implicit closer: Closer, protected val scheduler: Scheduler, iox: IOExecutor)
extends Actor with Stash with SimpleStateActor
{
  import agentConf.{akkaAskTimeout, journalMeta}
  import context.{actorOf, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate

  private val signatureVerifier = GenericSignatureVerifier(agentConf.config).orThrow
  private var persistence: JournaledStatePersistence[AgentState] = null
  private var recovered: Recovered[AgentState] = null
  private var eventWatch: JournalEventWatch = null
  private val started = SetOnce[Started]
  private val shutDownCommand = SetOnce[AgentCommand.ShutDown]
  private var isResetting = false
  private def terminating = shutDownCommand.isDefined
  private val terminateCompleted = Promise[Completed]()

  override def preStart() = {
    super.preStart()
    recovered = JournaledStateRecoverer.recover[AgentState](journalMeta, agentConf.config)
    eventWatch = recovered.eventWatch

    val sender = this.sender()
    JournaledStatePersistence
      .start(recovered, journalMeta, agentConf.journalConf, eventIdGenerator, keyedEventBus)
      .map { persistence =>
        watch(persistence.journalActor)
        this.persistence = persistence
      }
      .runToFuture
      .onComplete(tried =>
        (self ! Internal.JournalIsReady(tried))(sender))
  }

  override def postStop() = {
    if (eventWatch != null) eventWatch.close()
    super.postStop()
    terminatePromise.trySuccess(
      AgentTermination.Terminate(restart = shutDownCommand.toOption.fold(false)(_.restart)))
    if (isResetting) {
      logger.warn("DELETE JOURNAL FILES DUE TO AGENT RESET")
      journalMeta.deleteJournal(ignoreFailure = true)
    }
    logger.debug("Stopped")
  }

  def receive = {
    case Internal.JournalIsReady(Failure(throwable)) =>
      logger.error("JournalIsReady failed: " + throwable.toStringWithCauses)
      throw throwable

    case Internal.JournalIsReady(Success(())) =>
      val state = recovered.state
      if (state.isCreated) {
        addOrderKeeper(state.agentPath, state.controllerId).orThrow
      }
      become("startable")(startable)
      unstashAll()

    case _ =>
      stash()
  }

  private def startable: Receive = {
    case Input.Start =>
      become("ready")(ready)
      sender() ! Output.Ready
  }

  private def ready: Receive = {
    case cmd: Input.ExternalCommand =>
      executeExternalCommand(cmd)

    case Input.GetEventWatch =>
      if (!persistence.currentState.isCreated) {
        sender() ! Left(AgentNotCreatedProblem)
      } else {
        eventWatch.whenStarted.map(Right.apply) pipeTo sender()
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
        version = AgentStartInformation.PrettyVersion,
        buildId = AgentStartInformation.BuildId,
        startedAt = AgentStartInformation.StartedAt,
        isTerminating = terminating,
        system = systemInformation(),
        java = javaInformation)
  }

  private def executeExternalCommand(externalCommand: Input.ExternalCommand): Unit = {
    import externalCommand.{command, response}
    command match {
      case command: AgentCommand.ShutDown =>
        if (!terminating) {
          response.completeWith(terminateOrderKeeper(command))
        }

      case AgentCommand.Reset(agentRunId) =>
        persistence.currentState.checkAgentRunId(agentRunId) match {
          case Left(problem) => response.success(Left(problem))
          case Right(()) =>
            isResetting = true
            if (!terminating) {
              response.completeWith(terminateOrderKeeper(
                AgentCommand.ShutDown(processSignal = Some(SIGKILL),
                  suppressSnapshot = true, restart = true)))
            }
        }

      case AgentCommand.CreateAgent(agentPath, controllerId) if !terminating =>
        // Command is idempotent until AgentState has been touched
        val agentRunId = AgentRunId(persistence.journalId)
        persistence
          .persist(agentState =>
            if (!agentState.isCreated)
              Right((NoKey <-: AgentCreated(agentPath, agentRunId, controllerId)) :: Nil)
            else if (agentPath != agentState.agentPath)
              Left(AgentPathMismatchProblem(agentPath, agentState.agentPath))
            else if (controllerId != agentState.meta.controllerId)
              Left(AgentWrongControllerProblem(controllerId))
            else if (!agentState.isFreshlyCreated)
              Left(AgentAlreadyCreatedProblem)
            else
              Right(Nil))
          .flatMapT(eventAndState => Task {
            logger.info(s"Creating Agent '${agentPath.string}' for '$controllerId'")
            addOrderKeeper(agentPath, controllerId)
              .rightAs(eventAndState._2.eventId)
          })
          .runToFuture
          .onComplete { triedEventId =>
            response.complete(triedEventId.map(_.map(eventId =>
              AgentCommand.CreateAgent.Response(agentRunId, eventId))))
          }

      case AgentCommand.CoupleController(agentPath, agentRunId, eventId) if !terminating =>
        // Command does not change state. It only checks the coupling (for now)
        response.success(
          for {
            _ <- checkAgentPath(agentPath)
            _ <- persistence.currentState.checkAgentRunId(agentRunId)
            _ <- eventWatch.checkEventId(eventId)
          } yield
            CoupleController.Response(persistence.currentState.idToOrder.keySet))

      case command @ (_: AgentCommand.OrderCommand |
                      _: AgentCommand.TakeSnapshot |
                      _: AgentCommand.AttachItem |
                      _: AgentCommand.AttachSignedItem |
                      _: AgentCommand.DetachItem) =>
        // TODO Check AgentRunId ?
        started.toOption match {
          case None =>
            response.success(Left(AgentNotCreatedProblem))
          case Some(started) =>
            started.actor.forward(AgentOrderKeeper.Input.ExternalCommand(command, response))
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
    if (!agentState.isCreated)
      Left(AgentNotCreatedProblem)
    else if (requestedAgentPath != agentState.agentPath)
      Left(AgentPathMismatchProblem(requestedAgentPath, agentState.agentPath))
    else
      RightUnit
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
                executorConf,
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
    final case object Start
    final case class ExternalCommand(userId: UserId, command: AgentCommand, response: Promise[Checked[AgentCommand.Response]])
    case object GetEventWatch
  }

  object Output {
    case object Ready
  }

  private object Internal {
    final case class JournalIsReady(tried: Try[Unit])
  }

  private final case class Started(
    agentPath: AgentPath,
    controllerId: ControllerId,
    actor: ActorRef)

  @Singleton
  final class Factory @Inject private(
    clock: AlarmClock,
    agentConfiguration: AgentConfiguration,
    executorConf: JobExecutorConf,
    eventIdGenerator: EventIdGenerator,
    keyedEventBus: StampedKeyedEventBus)
    (implicit closer: Closer, scheduler: Scheduler, iox: IOExecutor)
  {
    def apply(terminatePromise: Promise[AgentTermination.Terminate]) =
      new AgentActor(terminatePromise, clock, agentConfiguration, executorConf,
        eventIdGenerator, keyedEventBus)
  }
}
