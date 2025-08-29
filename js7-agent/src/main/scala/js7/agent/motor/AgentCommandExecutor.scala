package js7.agent.motor

import cats.effect.{Deferred, IO, ResourceIO}
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.Response.Accepted
import js7.agent.data.commands.AgentCommand.{AttachItem, AttachSignedItem, Batch, ClusterSwitchOver, CoupleController, DedicateAgentDirector, DetachItem, EmergencyStop, IsOrderCommand, NoOperation, Reset, ResetSubagent, ShutDown, TakeSnapshot}
import js7.agent.data.event.AgentEvent.AgentDedicated
import js7.agent.motor.AgentCommandExecutor.*
import js7.agent.{CommandHandler, DirectorTermination}
import js7.base.auth.UserId
import js7.base.catsutils.CatsEffectExtensions.{catchAsChecked, catchIntoChecked, left, right, startAndForget}
import js7.base.catsutils.CatsExtensions.flatMapSome
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, CorrelIdWrapped, Log4j, Logger}
import js7.base.problem.{Checked, Problem}
import js7.base.service.{MainService, Service}
import js7.base.system.startup.Halt
import js7.base.time.AlarmClock
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, AsyncLock, ScalaUtils, SetOnce}
import js7.cluster.WorkingClusterNode
import js7.core.command.{CommandMeta, CommandRegister, CommandRun}
import js7.data.Problems.ClusterSwitchOverButNotCoupledProblem
import js7.data.agent.Problems.{AgentAlreadyDedicatedProblem, AgentIsShuttingDown, AgentNotDedicatedProblem, AgentPathMismatchProblem, AgentRunIdMismatchProblem, AgentWrongControllerProblem}
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.cluster.ClusterEvent.ClusterResetStarted
import js7.data.cluster.ClusterState
import js7.data.controller.{ControllerId, ControllerRunId}
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.subagent.SubagentId
import js7.subagent.Subagent
import org.apache.pekko.actor.ActorSystem

/** Starts and stops the AgentMotor and provides some Cluster and Journal operations. */
final class AgentCommandExecutor private(
  forDirector: Subagent.ForDirector,
  failedOverSubagentId: Option[SubagentId],
  workingClusterNode: WorkingClusterNode[AgentState],
  clock: AlarmClock,
  agentConf: AgentConfiguration,
  actorSystem: ActorSystem)
extends MainService, Service.StoppableByRequest, CommandHandler:

  protected type Termination = DirectorTermination

  private val journal = workingClusterNode.journal
  private val lock = AsyncLock()
  private val register = new CommandRegister[AgentCommand]

  private val _dedicated = Deferred.unsafe[IO, Dedicated]
  private val shutdownOnce = SetOnce[ShutDown]
  private val terminated = Deferred.unsafe[IO, DirectorTermination]
  private var isResetting = false

  private def isShuttingDown = shutdownOnce.isDefined

  protected def start =
    journal.aggregate.flatMap: agentState =>
      IO.whenA(agentState.isDedicated):
        startAgentMotor(agentState.agentPath, agentState.controllerId).map(_.orThrow)
    *>
      startService:
        untilStopRequested *>
          stopAgentMotor()

  def untilTerminated: IO[DirectorTermination] =
    logger.debugIO:
      terminated.get

  def execute(command: AgentCommand, meta: CommandMeta): IO[Checked[AgentCommand.Response]] =
    executeCommand(command, meta)

  private def executeCommand(
    command: AgentCommand,
    meta: CommandMeta,
    batchId: Option[CorrelId] = None)
  : IO[Checked[AgentCommand.Response]] =
    register.resource[IO](command, meta, CorrelId.current, batchId).use: run =>
      logCommand(run):
        executeCommand2(command, run.correlId, meta, batchId = None)

  private def logCommand[A](run: CommandRun[AgentCommand])(body: IO[A]): IO[A] =
    run.command match
      case _: AgentCommand.Batch => body // Log only individual commands
      case _ => logger.debugIO(run.toString)(body)

  private def executeCommand2(
    command: AgentCommand,
    correlId: CorrelId,
    meta: CommandMeta,
    batchId: Option[CorrelId])
  : IO[Checked[AgentCommand.Response]] =
    command match
      case cmd @ (_: IsOrderCommand | _: AttachItem | _: AttachSignedItem | _: DetachItem |
                  _: ResetSubagent) =>
        // TODO Check AgentRunId ?
        checkedAgentMotor.flatMapT:
          _.executeCommand(cmd)

      case DedicateAgentDirector(directors, controllerId, controllerRunId, agentPath) =>
        if isShuttingDown then
          IO.left(AgentIsShuttingDown)
        else
          // Command is idempotent until AgentState has been touched
          dedicate(directors, controllerId, controllerRunId, agentPath)
            .map(_.map: (agentRunId, eventId) =>
              AgentCommand.DedicateAgentDirector.Response(agentRunId, eventId))

      case CoupleController(agentPath, agentRunId, eventId, controllerRunId) =>
        if isShuttingDown then
          IO.left(AgentIsShuttingDown)
        else
          // Command does not change AgentState. It only checks the coupling (for now)
          IO:
            for
              _ <- checkControllerRunId(controllerRunId)
              _ <- checkAgentPath(agentPath)
              _ <- checkAgentRunId(agentRunId)
              _ <- journal.eventWatch.checkEventId(eventId)
            yield
              CoupleController.Response(
                orderIds = journal.unsafeAggregate().idToOrder.keySet)

      case cmd: Reset =>
        reset(cmd)

      case TakeSnapshot =>
        journal.takeSnapshot
          .as(Right(Accepted))

      case cmd: ClusterSwitchOver =>
        switchOver(cmd)

      case cmd: ShutDown =>
        initiateShutdown(cmd)

      case NoOperation =>
        IO.right(Accepted)

      case EmergencyStop(restart) =>
        Halt.haltJava("🟥 EmergencyStop command received: JS7 AGENT STOPS NOW", restart = restart)

      case Batch(wrappedCommands) =>
        // Execute one command after the other
        fs2.Stream.iterable(wrappedCommands)
          .evalMap: wrapped =>
            val CorrelIdWrapped(subcorrelId, cmd) = wrapped
            subcorrelId.orNew.bind:
              executeCommand(cmd, meta, batchId orElse Some(correlId))
          .compile
          .toVector
          .map(AgentCommand.Batch.Response(_))
          .catchAsChecked

  private def dedicate(
    directors: Seq[SubagentId],
    controllerId: ControllerId,
    controllerRunId: ControllerRunId,
    agentPath: AgentPath)
  : IO[Checked[(AgentRunId, EventId)]] =
    IO.defer:
      // Command is idempotent until AgentState has been touched
      val agentRunId = AgentRunId(journal.journalId)
      journal.persist: agentState =>
        if !agentState.isDedicated
          || agentPath == agentState.agentPath
          && agentState.meta.directors != directors then
          UserId.checked(agentPath.string).map: _ => /*used for Subagent login*/
            (NoKey <-: AgentDedicated(directors, agentPath, agentRunId, controllerId, Some(controllerRunId)))
              :: Nil
        else if agentPath != agentState.agentPath then
          Left(AgentPathMismatchProblem(agentPath, agentState.agentPath))
        else if controllerId != agentState.meta.controllerId then
          Left(AgentWrongControllerProblem(controllerId, agentState.meta.controllerId))
        else if !agentState.isFreshlyDedicated
          && !agentState.meta.controllerRunId.contains(controllerRunId) then
          Left(AgentAlreadyDedicatedProblem)
        else
          Right(Nil)
      .flatMapT: persisted =>
        IO.defer:
          logger.info(s"Dedicating $agentPath to '$controllerId'")
          Log4j.set("js7.serverId", agentPath.toString)
          startAgentMotor(agentPath, controllerId)
            .rightAs(agentRunId -> persisted.aggregate.eventId)
            .flatTapT: _ =>
              IO.whenA(isStopping): // Shutdown while being dedicated?
                stopAgentMotor()
              .map(Right(_))

  private def reset(cmd: Reset): IO[Checked[Accepted]] =
    cmd.agentRunId.fold(Checked.unit)(checkAgentRunId(_)) match
      //? case Left(AgentNotDedicatedProblem) => IO.right(Accepted)
      case Left(problem) => IO.left(problem)
      case Right(()) =>
        logger.info(s"❗ $cmd")
        isResetting = true
        if isShuttingDown then
          IO.left(Problem.pure("Director is shutting down"))
        else
          agentMotor.flatMapSome: agentMotor =>
            agentMotor.resetAllSubagents
          *>
            journal.persist:
              _.clusterState.isSubtypeOf[ClusterState.Coupled] ?
                // ClusterResetStarted lets the passive cluster node delete its journal
                ClusterResetStarted
            .catchIntoChecked
            .flatMapT: _ =>
              initiateShutdown(ShutDown(
                processSignal = Some(SIGKILL), suppressSnapshot = true, restart = true))
            .flatTapT: _ =>
              journal.deleteJournalWhenStopping.as(Checked.unit)

  private def switchOver(cmd: ClusterSwitchOver): IO[Checked[AgentCommand.Response]] =
    IO.defer:
      logger.info(s"❗️ $cmd")
      // SubagentKeeper stops the local (surrounding) Subagent,
      // which lets the Director (RunningAgent) stop
      initiateShutdown:
        ShutDown(
          clusterAction = Some(ShutDown.ClusterAction.Switchover),
          restartDirector = true)

  private def initiateShutdown(cmd: ShutDown): IO[Checked[Accepted]] =
    logger.traceIO(cmd.toString):
      IO.defer:
        if !shutdownOnce.trySet(cmd) then
          IO.left(AgentIsShuttingDown)
        else
          IO.whenA(cmd.suppressSnapshot):
            IO(journal.suppressSnapshotWhenStopping())
          .productR:
            if cmd.isSwitchover then
              journal.aggregate.map(_.clusterState).map:
                case coupled: ClusterState.Coupled => Right(())
                case clusterState => Left(ClusterSwitchOverButNotCoupledProblem(clusterState))
            else
              IO.right(())
          .flatMapT { _ =>
            // Run asynchronously as a fiber:
            locally:
              if cmd.isFailover then
                agentMotor.flatMapSome(_.kill) *>
                  journal.kill
              else if cmd.isSwitchover then
                stopAgentMotor(cmd) *>
                  workingClusterNode.switchOver
              else
                stopAgentMotor(cmd)
            .productR:
              stop.productR:
                terminated.complete:
                  // If dedicated while being shut down, the dedicated AgentMotor is
                  // being stopped asynchronously in background.
                  DirectorTermination(
                    restartJvm = cmd.restart,
                    restartDirector = cmd.restartDirector)
            .startAndForget // Respond early to the command
            .as(Right(Accepted))
          }

  private def startAgentMotor(agentPath: AgentPath, controllerId: ControllerId): IO[Checked[Unit]] =
    lock.lock:
      IO.defer:
        if isShuttingDown then
          IO.left(AgentIsShuttingDown)
        else
          _dedicated.tryGet.flatMap:
            case Some(dedicated: Dedicated) =>
              val agentMotor = dedicated.agentMotor
              if agentMotor.agentPath == agentPath && agentMotor.controllerId == controllerId then
                logger.debug("❓ startAgentMotor: already started")
                IO.right(())
              else
                IO.left(Problem(s"This Agent has already been dedicated as '${agentMotor.agentPath
                  }' for '${agentMotor.controllerId}'"))

            case None =>
              AgentMotor.service(failedOverSubagentId, forDirector, workingClusterNode,
                  agentConf, actorSystem)
                .toAllocated
                .flatMap: allocated =>
                  _dedicated.complete(Dedicated(allocated))
                    .as(Checked.unit)

  private def stopAgentMotor(cmd: AgentCommand.ShutDown = AgentCommand.ShutDown()): IO[Unit] =
    logger.traceIO:
      agentMotor.flatMapSome:
        _.shutdown(cmd)
      .productR:
        _dedicated.tryGet.flatMapSome:
          _.stopAgentMotor // stop the AgentMotor service
        .void

  private def agentMotor: IO[Option[AgentMotor]] =
    _dedicated.tryGet.map(_.map(_.agentMotor))

  private def checkedAgentMotor: IO[Checked[AgentMotor]] =
    _dedicated.tryGet.flatMap:
      case None => IO.left(AgentNotDedicatedProblem)
      case Some(dedicated) => IO.right(dedicated.agentMotor)

  private def checkAgentPath(requestedAgentPath: AgentPath): Checked[Unit] =
    val agentState = journal.unsafeAggregate()
    if !agentState.isDedicated then
      Left(AgentNotDedicatedProblem)
    else if requestedAgentPath != agentState.agentPath then
      Left(AgentPathMismatchProblem(requestedAgentPath, agentState.agentPath))
    else
      Checked.unit

  private def checkAgentRunId(requestedAgentRunId: AgentRunId): Checked[Unit] =
    val agentState = journal.unsafeAggregate()
    if !agentState.isDedicated then
      Left(AgentNotDedicatedProblem)
    else if requestedAgentRunId != agentState.meta.agentRunId then
      val problem = AgentRunIdMismatchProblem(agentState.meta.agentPath)
      logger.warn(
        s"$problem, requestedAgentRunId=$requestedAgentRunId, agentRunId=${agentState.meta.agentRunId}")
      Left(problem)
    else
      Checked.unit

  private def checkControllerRunId(requestedControllerRunId: ControllerRunId): Checked[Unit] =
    val agentState = journal.unsafeAggregate()
    if !agentState.isDedicated then
      Left(AgentNotDedicatedProblem)
    else if agentState.meta.controllerRunId.exists(_ != requestedControllerRunId) then
      val problem = Problem("ControllerRunId does not match")
      logger.warn(
        s"$problem, requestedControllerRunId=$requestedControllerRunId, controllerRunId=${agentState.meta.controllerRunId}")
      Left(problem)
    else
      Checked.unit

  override def toString = "AgentCommandExecutor"


object AgentCommandExecutor:
  private val logger = Logger[this.type]

  def service(
    forDirector: Subagent.ForDirector,
    workingClusterNode: WorkingClusterNode[AgentState],
    clock: AlarmClock,
    agentConf: AgentConfiguration,
    actorSystem: ActorSystem)
  : ResourceIO[AgentCommandExecutor] =
    val journal = workingClusterNode.journal
    Service.resource:
      journal.aggregate.map: agentState =>
        workingClusterNode.failedNodeId.map: nodeId =>
          agentState.meta.clusterNodeIdToSubagentId(nodeId).orThrow
      .flatMap: failedOverSubagentId =>
        IO:
          new AgentCommandExecutor(forDirector, failedOverSubagentId, workingClusterNode, clock,
            agentConf, actorSystem)


  private final case class Dedicated(
    private val allocated: Allocated[IO, AgentMotor]):

    def agentMotor = allocated.allocatedThing

    def stopAgentMotor = allocated.release
