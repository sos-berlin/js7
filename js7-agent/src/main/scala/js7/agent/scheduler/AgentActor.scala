package js7.agent.scheduler

import akka.actor.{Actor, ActorRef, Props, Stash, Terminated}
import akka.pattern.ask
import cats.syntax.traverse.*
import java.util.Objects.requireNonNull
import js7.agent.DirectorTermination
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.CoupleController
import js7.agent.data.event.AgentEvent.AgentDedicated
import js7.agent.scheduler.AgentActor.*
import js7.agent.scheduler.order.AgentOrderKeeper
import js7.base.auth.UserId
import js7.base.generic.Completed
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.RichCheckedTask
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.AlarmClock
import js7.base.utils.ScalaUtils.RightUnit
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, SetOnce}
import js7.base.web.Uri
import js7.cluster.ClusterNode
import js7.common.akkautils.{SimpleStateActor, SupervisorStrategies}
import js7.data.agent.Problems.{AgentAlreadyDedicatedProblem, AgentIsShuttingDown, AgentNotDedicatedProblem, AgentPathMismatchProblem, AgentRunIdMismatchProblem, AgentWrongControllerProblem}
import js7.data.agent.{AgentPath, AgentRef, AgentRunId}
import js7.data.cluster.ClusterEvent.ClusterResetStarted
import js7.data.cluster.ClusterState
import js7.data.cluster.ClusterState.HasNodes
import js7.data.controller.{ControllerId, ControllerRunId}
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.BasicItemEvent.ItemAttachedToMe
import js7.data.node.NodeId
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.state.FileJournal
import js7.subagent.Subagent
import js7.subagent.director.RemoteSubagentDriver
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private[agent] final class AgentActor(
  forDirector: Subagent.ForDirector,
  failedOverSubagentId: Option[SubagentId],
  clusterNode: ClusterNode[AgentState],
  terminatePromise: Promise[DirectorTermination],
  journalAllocated: Allocated[Task, FileJournal[AgentState]],
  clock: AlarmClock,
  agentConf: AgentConfiguration)
  (implicit protected val scheduler: Scheduler)
extends Actor with Stash with SimpleStateActor
{
  import agentConf.{implicitAkkaAskTimeout, journalLocation}
  import context.{actorOf, watch}
  val journal = journalAllocated.allocatedThing
  import journal.eventWatch

  override val supervisorStrategy = SupervisorStrategies.escalate

  private var recoveredAgentState: AgentState = null
  private val dedicated = SetOnce[Dedicated]
  private val shutDownOnce = SetOnce[AgentCommand.ShutDown]
  private var isResetting = false
  private def terminating = shutDownOnce.isDefined
  private val terminateCompleted = Promise[Completed]()

  override def preStart() = {
    watch(journal.journalActor)
    super.preStart()
  }

  override def postStop() =
    logger.debugCall {
      super.postStop()
      if (isResetting) {
        journalLocation.deleteJournal(ignoreFailure = true)
      }
      terminatePromise.trySuccess(
        DirectorTermination(
          restartJvm = shutDownOnce.toOption.fold(false)(_.restart),
          restartDirector = shutDownOnce.toOption.fold(false)(_.restartDirector)))
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

    case ContinueReset(response) =>
      response.completeWith(terminateOrderKeeper(
        AgentCommand.ShutDown(processSignal = Some(SIGKILL),
          suppressSnapshot = true, restart = true)))

    case Terminated(a) if dedicated.toOption.exists(_.actor == a) =>
      logger.debug("AgentOrderKeeper terminated")
      context.stop(self)

    case Terminated(actor) if actor == journal.journalActor /*&& terminating*/ =>
      if (!terminating) {
        // SwitchOver lets AgentOrderKeeper kill the JournalActor
        logger.error("JournalActor terminated unexpectedly")
        context.stop(self)
      } else {
        for (_ <- terminateCompleted.future) {
          context.stop(self)
        }
      }
  }

  private def executeExternalCommand(externalCommand: Input.ExternalCommand): Unit = {
    import externalCommand.{command, response}
    command match {
      case command: AgentCommand.ShutDown =>
        response.completeWith(
          if (terminating)
            Future.successful(Right(AgentCommand.Response.Accepted))
          else
            terminateOrderKeeper(command))

      case AgentCommand.Reset(maybeAgentRunId) =>
        maybeAgentRunId.fold(Checked.unit)(checkAgentRunId(_)) match {
          case Left(AgentNotDedicatedProblem) =>
            response.success(Right(AgentCommand.Response.Accepted))

          case Left(problem) =>
            response.success(Left(problem))

          case Right(()) =>
            logger.info(s"❗ $command")
            isResetting = true
            if (terminating) {
              response.success(Right(AgentCommand.Response.Accepted/*???*/))
            } else {
              dedicated.toOption
                .fold(Task.unit)(dedicated => Task
                  .fromFuture(
                    (dedicated.actor ? AgentOrderKeeper.Input.ResetAllSubagents)(
                      RemoteSubagentDriver.subagentResetTimeout))
                  .void)
                .*>(journal
                  .persist(_.clusterState match {
                    case _: ClusterState.Coupled =>
                      // Is it a good idea to persist something when Agent must be reset ???
                      Right((NoKey <-: ClusterResetStarted) :: Nil)
                    case _ => Right(Nil)
                  })
                  .materializeIntoChecked
                  .flatMap {
                    case Left(problem) => Task(response.success(Left(problem)))
                    case Right(_) =>
                      Task.right {
                        self ! ContinueReset(response)
                      }
                  })
                .runToFuture
            }
        }

      case AgentCommand.DedicateAgentDirector(directors, controllerId, controllerRunId, agentPath)
        if !terminating =>
        // Command is idempotent until AgentState has been touched
        dedicate(directors, controllerId, controllerRunId, agentPath)
          .runToFuture
          .onComplete { tried =>
            response.complete(
              tried.map(_.map { case (agentRunId, eventId) =>
                AgentCommand.DedicateAgentDirector.Response(agentRunId, eventId)
              }))
          }

      case AgentCommand.CoupleController(agentPath, agentRunId, eventId, controllerRunId: ControllerRunId)
        if !terminating =>
        // Command does not change state. It only checks the coupling (for now)
        response.success(
          for {
            _ <- checkControllerRunId(controllerRunId)
            _ <- checkAgentPath(agentPath)
            _ <- checkAgentRunId(agentRunId)
            _ <- eventWatch.checkEventId(eventId)
          } yield
            CoupleController.Response(journal.unsafeCurrentState().idToOrder.keySet))

      case command @ (_: AgentCommand.OrderCommand |
                      _: AgentCommand.TakeSnapshot |
                      _: AgentCommand.AttachItem |
                      _: AgentCommand.AttachSignedItem |
                      _: AgentCommand.DetachItem |
                      _: AgentCommand.ResetSubagent |
                      _: AgentCommand.ClusterSwitchOver) =>
        // TODO Check AgentRunId ?
        dedicated.toOption match {
          case None =>
            response.success(Left(AgentNotDedicatedProblem))
          case Some(dedicated) =>
            dedicated.actor.forward(
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

  private def dedicate(
    directors: Seq[SubagentId],
    controllerId: ControllerId,
    controllerRunId: ControllerRunId,
    agentPath: AgentPath)
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
                AgentDedicated(directors, agentPath, agentRunId, controllerId, Some(controllerRunId)))
          else if (agentPath != agentState.agentPath)
            Left(AgentPathMismatchProblem(agentPath, agentState.agentPath))
          else if (controllerId != agentState.meta.controllerId)
            Left(AgentWrongControllerProblem(controllerId, agentState.meta.controllerId))
          else if (!agentState.isFreshlyDedicated
            && !agentState.meta.controllerRunId.contains(controllerRunId))
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

  private def checkControllerRunId(requestedControllerRunId: ControllerRunId): Checked[Unit] = {
    val agentState = journal.unsafeCurrentState()
    if (!agentState.isDedicated)
      Left(AgentNotDedicatedProblem)
    else if (agentState.meta.controllerRunId.exists(_ != requestedControllerRunId)) {
      val problem = Problem("ControllerRunId does not match")
      logger.warn(
        s"$problem, requestedControllerRunId=$requestedControllerRunId, controllerRunId=${agentState.meta.controllerRunId}")
      Left(problem)
    } else
      Checked.unit
  }

  private def continueTermination(): Unit =
    if (terminating) {
      if (dedicated.isEmpty) {
        // When no AgentOrderKeeper has been dedicated, we need to stop the journal ourselve
        //journal.journalActor ! JournalActor.Input.Terminate
        context.stop(self)
      }
    }

  private def terminateOrderKeeper(shutDown: AgentCommand.ShutDown)
  : Future[Checked[AgentCommand.Response.Accepted]] = {
    logger.trace("terminateOrderKeeper")
    if (!shutDownOnce.trySet(shutDown))
      Future.successful(Left(AgentDirectorIsShuttingDownProblem))
    else
      dedicated.toOption match {
        case None =>
          continueTermination()
          Future.successful(Right(AgentCommand.Response.Accepted))

        case (Some(dedicated)) =>
          terminateAgentOrderKeeper(dedicated, shutDown)
      }
  }

  private def terminateAgentOrderKeeper(dedicated: Dedicated, shutDown: AgentCommand.ShutDown)
  : Future[Checked[AgentCommand.Response.Accepted]] =
    (dedicated.actor ? shutDown)
      .mapTo[AgentCommand.Response.Accepted]
      .map { ordersTerminated =>
        terminateCompleted.success(Completed) // Wait for child Actor termination
        continueTermination()
        Right(ordersTerminated)
      }

  private def addOrderKeeper(agentPath: AgentPath, controllerId: ControllerId): Checked[Unit] =
    synchronized {
      if (terminating)
        Left(AgentDirectorIsShuttingDownProblem)
      else
        dedicated.toOption match {
          case Some(Dedicated(`agentPath`, `controllerId`, _)) =>
            logger.debug("❓ Already dedicated")
            Checked.unit

          case Some(dedicated) =>
            Left(Problem(
              s"This Agent has already been dedicated as '${dedicated.agentPath}' for '${dedicated.controllerId}'"))

          case None =>
            val recoveredAgentState = this.recoveredAgentState
            this.recoveredAgentState = null  // release memory
            val actor = actorOf(
              Props {
                new AgentOrderKeeper(
                  forDirector,
                  clusterNode,
                  failedOverSubagentId,
                  requireNonNull(recoveredAgentState),
                  changeSubagentAndClusterNode,
                  journalAllocated,
                  shutDownOnce,
                  clock,
                  agentConf)
                },
              "AgentOrderKeeper")
            watch(actor)
            dedicated := Dedicated(agentPath, controllerId, actor)
            Checked.unit
        }
    }

  /** Emits the event, and ClusterSettingUpdated if needed, in separate transaction. */
  private def changeSubagentAndClusterNode(event: ItemAttachedToMe): Task[Checked[Unit]] =
    logger.debugTask(journal.state
      .flatMap(agentState =>
        if (!agentState.isDedicated)
          journal.persistKeyedEvent(event).rightAs(())
        else
          Task.pure(agentState.applyEvent(event)).flatMapT(nextAgentState =>
            demandedClusterNodeUris(nextAgentState) match {
              case None =>
                journal.persistKeyedEvent(event).rightAs(())

              case Some(idToUri) =>
                agentState.clusterState
                  .match_ {
                    case ClusterState.Empty =>
                      Some(NodeId.primary)
                    case clusterState: HasNodes =>
                      (clusterState.setting.idToUri != idToUri) ? clusterState.activeId
                  }
                  .fold(journal.persistKeyedEvent(event).rightAs(()))(activeNodeId =>
                    Task(clusterNode.workingClusterNode)
                      .flatMapT(_
                        .appointNodes(idToUri, activeNodeId, extraEvent = Some(event))))
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
        NodeId.primary -> subagentItems(0).uri,
        NodeId.backup -> subagentItems(1).uri)


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

  private final case class Dedicated(
    agentPath: AgentPath,
    controllerId: ControllerId,
    actor: ActorRef)

  private case class ContinueReset(response: Promise[Checked[AgentCommand.Response]])

  private case object AgentDirectorIsShuttingDownProblem extends Problem.ArgumentlessCoded
}
