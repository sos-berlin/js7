package js7.agent.scheduler

import akka.actor.{Actor, ActorRef, PoisonPill, Props, Stash, Terminated}
import akka.pattern.{ask, pipe}
import com.softwaremill.diffx.generic.auto._
import java.util.Objects.requireNonNull
import javax.inject.{Inject, Singleton}
import js7.agent.configuration.{AgentConfiguration, AgentStartInformation}
import js7.agent.data.Problems.{AgentIsShuttingDown, AgentNotCreatedProblem, AgentPathMismatchProblem, AgentRunIdMismatchProblem}
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.CoupleController
import js7.agent.data.event.AgentControllerEvent.AgentCreated
import js7.agent.data.views.AgentOverview
import js7.agent.data.{AgentState, AgentTermination}
import js7.agent.scheduler.AgentActor._
import js7.agent.scheduler.job.JobActor
import js7.agent.scheduler.order.AgentOrderKeeper
import js7.base.auth.UserId
import js7.base.generic.Completed
import js7.base.io.file.FileUtils.syntax._
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.thread.IOExecutor
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.{Closer, SetOnce}
import js7.common.akkautils.{SimpleStateActor, SupervisorStrategies}
import js7.common.crypt.generic.GenericSignatureVerifier
import js7.common.system.JavaInformations.javaInformation
import js7.common.system.SystemInformations.systemInformation
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.event.EventId
import js7.executor.configuration.JobExecutorConf
import js7.journal.data.JournalMeta
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
  agentConf: AgentConfiguration,
  executorConf: JobExecutorConf,
  eventIdGenerator: EventIdGenerator,
  keyedEventBus: StampedKeyedEventBus)
  (implicit closer: Closer, protected val scheduler: Scheduler, iox: IOExecutor)
extends Actor with Stash with SimpleStateActor
{
  import agentConf.{akkaAskTimeout, stateDirectory}
  import context.{actorOf, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate

  private val signatureVerifier = GenericSignatureVerifier(agentConf.config).orThrow
  private var persistence: JournaledStatePersistence[AgentState] = null
  private var recovered: Recovered[AgentState] = null
  private var eventWatch: JournalEventWatch = null
  private val agentOrderKeeperActor = SetOnce[ActorRef]
  private val shutDownCommand = SetOnce[AgentCommand.ShutDown]
  private def terminating = shutDownCommand.isDefined
  private val terminateCompleted = Promise[Completed]()

  override def preStart() = {
    super.preStart()
    val journalMeta = JournalMeta(AgentState, stateDirectory / "agent")
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
    logger.debug("Stopped")
  }

  def receive = {
    case Internal.JournalIsReady(Failure(throwable)) =>
      logger.error("JournalIsReady failed: " + throwable.toStringWithCauses)
      throw throwable

    case Internal.JournalIsReady(Success(())) =>
      val state = recovered.state
      if (state.isCreated) {
        addOrderKeeper()
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

    case msg: JobActor.Output.ReadyForOrder.type =>
      for (actor <- agentOrderKeeperActor) {
        actor.forward(msg)
      }

    case Terminated(a) if agentOrderKeeperActor contains a =>
      logger.debug("AgentOrderKeeper terminated")
      continueTermination()

    case Terminated(actor) if actor == persistence.journalActor && terminating =>
      for (_ <- terminateCompleted.future) context.self ! PoisonPill

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
          shutDownCommand := command
          terminateOrderKeeper(command) onComplete { ordersTerminated =>
            response.complete(ordersTerminated map Right.apply)
            terminateCompleted.success(Completed)  // Wait for child Actor termination
            continueTermination()
          }
        }

      case AgentCommand.CreateAgent(controllerId, agentPath) if !terminating =>
        if (!persistence.currentState.isCreated) {
          val agentRunId = AgentRunId(persistence.journalId)
          persistence.persistKeyedEvent(AgentCreated(agentPath, agentRunId, controllerId))
            .tapEval(checked => Task {
              if (checked.isRight) {
                addOrderKeeper()
              }
            })
            .runToFuture
            .onComplete { tried =>
              response.complete(tried.map(_.map(_ => AgentCommand.CreateAgent.Response(agentRunId))))
            }
        } else {
          response.success(
            checkAgentPath(agentPath, EventId.BeforeFirst)
              .map(_ => AgentCommand.CreateAgent.Response(persistence.currentState.meta.agentRunId)))
        }

      case AgentCommand.CoupleController(agentPath, agentRunId, eventId) if !terminating =>
        // Command does not change state. It only checks the coupling (for now)
        response.success(
         if (agentRunId != persistence.currentState.meta.agentRunId)
            Left(AgentRunIdMismatchProblem(agentPath))
         else
           for (_ <- checkAgentPath(agentPath, eventId)) yield
             CoupleController.Response(persistence.currentState.idToOrder.keySet))

      case command @ (_: AgentCommand.OrderCommand |
                      _: AgentCommand.TakeSnapshot |
                      _: AgentCommand.AttachItem |
                      _: AgentCommand.AttachSignedItem |
                      _: AgentCommand.DetachItem) =>
        // TODO Check AgentRunId ?
        agentOrderKeeperActor.toOption match {
          case None =>
            response.success(Left(AgentNotCreatedProblem))
          case Some(actor) =>
            actor.forward(AgentOrderKeeper.Input.ExternalCommand(command, response))
        }

      case command =>
        response.failure(
          if (terminating)
            AgentIsShuttingDown.throwable
          else
            new RuntimeException(s"Unexpected command for AgentActor: $command"))
    }
  }

  private def checkAgentPath(requestedAgentPath: AgentPath, eventId: EventId): Checked[Unit] = {
    val agentState = persistence.currentState
    if (!agentState.isCreated)
      Left(AgentNotCreatedProblem)
    else if (requestedAgentPath != agentState.agentPath)
      Left(AgentPathMismatchProblem(requestedAgentPath, agentState.agentPath))
    else
      eventWatch.checkEventId(eventId)
  }

  private def continueTermination(): Unit =
    if (terminating) {
      if (agentOrderKeeperActor.isEmpty) {
        // When no AgentOrderKeeper has been startet, we need to stop the journal ourselves
        //persistence.journalActor ! JournalActor.Input.Terminate
        persistence.stop.runAsyncAndForget
      }
    }

  private def terminateOrderKeeper(shutDown: AgentCommand.ShutDown): Future[AgentCommand.Response.Accepted] =
    agentOrderKeeperActor.toOption match {
      case None => Future.successful(AgentCommand.Response.Accepted)
      case Some(actor) => (actor ? shutDown).mapTo[AgentCommand.Response.Accepted]
    }

  private def addOrderKeeper(): ActorRef = {
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
          agentConf)
        },
      "AgentOrderKeeper")
    watch(actor)
    agentOrderKeeperActor := actor
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

  @Singleton
  final class Factory @Inject private(
    agentConfiguration: AgentConfiguration,
    executorConf: JobExecutorConf,
    eventIdGenerator: EventIdGenerator,
    keyedEventBus: StampedKeyedEventBus)
    (implicit closer: Closer, scheduler: Scheduler, iox: IOExecutor)
  {
    def apply(terminatePromise: Promise[AgentTermination.Terminate]) =
      new AgentActor(terminatePromise, agentConfiguration, executorConf, eventIdGenerator, keyedEventBus)
  }
}
