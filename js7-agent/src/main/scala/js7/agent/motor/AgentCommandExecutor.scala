package js7.agent.motor

import cats.effect.std.AtomicCell
import cats.effect.{Deferred, IO, ResourceIO}
import cats.syntax.option.none
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
import js7.base.log.log4j.Log4j
import js7.base.log.{CorrelId, CorrelIdWrapped, Logger}
import js7.base.problem.{Checked, Problem}
import js7.base.service.{MainService, Service}
import js7.base.system.startup.Halt
import js7.base.utils.CatsUtils.syntax.{RichResource, logWhenItTakesLonger}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, AsyncLock, ScalaUtils}
import js7.cluster.WorkingClusterNode
import js7.core.command.{CommandMeta, CommandRegister}
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
  agentConf: AgentConfiguration,
  actorSystem: ActorSystem,
  shuttingDown: AtomicCell[IO, Option[ShutDown]])
extends
  MainService, Service.StoppableByRequest, CommandHandler:

  protected type Termination = DirectorTermination

  private val journal = workingClusterNode.journal
  private val lock = AsyncLock()
  private val register = new CommandRegister[AgentCommand]

  private val _dedicated = Deferred.unsafe[IO, Dedicated]
  private val terminated = Deferred.unsafe[IO, DirectorTermination]

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
      command match
        case Batch(wrappedCommands) =>
          // Execute one command after the other
          fs2.Stream.iterable(wrappedCommands)
            .evalMap: wrapped =>
              val CorrelIdWrapped(subcorrelId, cmd) = wrapped
              subcorrelId.orNew.bind:
                executeCommand(cmd, meta, batchId orElse Some(run.correlId))
            .compile
            .toVector
            .map(AgentCommand.Batch.Response(_))
            .catchAsChecked

        case command: AgentCommand.NonBatch =>
          logger.debugIO(run.toString):
            executeCommand2(command, run.correlId, meta, batchId = None)
          .logWhenItTakesLonger(run.toString)

  private def executeCommand2(
    command: AgentCommand.NonBatch,
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

      case DedicateAgentDirector(agentPath, directors, controllerId, controllerRunId) =>
        whenNotShuttingDown:
          // Command is idempotent until AgentState has been touched
          dedicate(agentPath, directors, controllerId, controllerRunId)
            .map(_.map: (agentRunId, eventId) =>
              AgentCommand.DedicateAgentDirector.Response(agentRunId, eventId))

      case CoupleController(agentPath, agentRunId, eventId, controllerRunId) =>
        whenNotShuttingDown:
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
        reset(cmd, meta)

      case TakeSnapshot =>
        journal.takeSnapshot
          .as(Right(Accepted))

      case cmd: ClusterSwitchOver =>
        switchOver(cmd, meta)

      case cmd: ShutDown =>
        initiateShutdown(cmd, cmd, meta)

      case NoOperation =>
        IO.right(Accepted)

      case EmergencyStop(restart) =>
        Halt.haltJava("🟥 EmergencyStop command received: JS7 AGENT DIRECTOR STOPS NOW", restart = restart)

  private def dedicate(
    agentPath: AgentPath,
    directors: Seq[SubagentId],
    controllerId: ControllerId,
    controllerRunId: ControllerRunId)
  : IO[Checked[(AgentRunId, EventId)]] =
    IO.defer:
      // Command is idempotent until AgentState has been touched
      val agentRunId = AgentRunId(journal.journalId)
      journal.persist: agentState =>
        if !agentState.isDedicated
          || agentPath == agentState.agentPath
          && agentState.meta.directors != directors then
          UserId.checked(agentPath.string).map: _ => /*used for Subagent login*/
            val event = AgentDedicated(agentPath, directors, agentRunId,
              controllerId, Some(controllerRunId))
            (NoKey <-: event) :: Nil
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

  private def reset(cmd: Reset, meta: CommandMeta): IO[Checked[Accepted]] =
    cmd.agentRunId.fold(Checked.unit)(checkAgentRunId(_)) match
      //? case Left(AgentNotDedicatedProblem) => IO.right(Accepted)
      case Left(problem) => IO.left(problem)
      case Right(()) =>
        logger.info(s"❗ $cmd • $meta")
        whenNotShuttingDown:
          agentMotor.flatMapSome: agentMotor =>
            agentMotor.resetBareSubagents
          *>
            journal.persist:
              _.clusterState.isSubtypeOf[ClusterState.Coupled] ?
                // ClusterResetStarted lets the passive cluster node delete its journal
                ClusterResetStarted
            .catchIntoChecked
            .flatMapT: _ =>
              initiateShutdown(
                cmd,
                ShutDown(processSignal = Some(SIGKILL), suppressSnapshot = true, restart = true),
                meta)
            .flatTapT: _ =>
              journal.deleteJournalWhenStopping.as(Checked.unit)

  private def switchOver(cmd: ClusterSwitchOver, meta: CommandMeta): IO[Checked[AgentCommand.Response]] =
    // SubagentKeeper stops the local (surrounding) Subagent,
    // which lets the Director (RunningAgent) stop
    initiateShutdown(
      cmd,
      ShutDown(
        clusterAction = Some(ShutDown.ClusterAction.Switchover),
        restartDirector = true),
      meta)

  private def initiateShutdown(originalCmd: ShutDown | ClusterSwitchOver | Reset, cmd: ShutDown, meta: CommandMeta)
  : IO[Checked[Accepted]] =
    logger.traceIO("initiateShutdown", cmd):
      locally:
        if cmd.isSwitchover then
          journal.aggregate.map(_.clusterState).map:
            case coupled: ClusterState.Coupled => Right(())
            case clusterState => Left(ClusterSwitchOverButNotCoupledProblem(clusterState))
        else
          IO.right(())
      .flatMapT: _ =>
        shuttingDown.getAndSet(Some(cmd)).flatMap:
          case None =>
            logger.info(s"❗ Shutdown  due to $originalCmd • $meta")
            shutdown(cmd)
              .pipeIf(!cmd.restartDirector/*not switchover?*/):
                _.startAndForget // Shutdown in background and respond the command early
              .as(Right(Accepted))

          case Some(runningShutdown) =>
            cmd.processSignal.foldMap:
              forDirector.subagent.killAllProcesses
            .productR:
              // Update parameters used for ProgramTermination
              shuttingDown.update(_.map: runningShutdown =>
                runningShutdown.copy(
                  restart = runningShutdown.restart | cmd.restart,
                  restartDirector = (runningShutdown.restartDirector | cmd.restartDirector)
                    && !(runningShutdown.restart | cmd.restart)))
            .as:
              Right(Accepted)

  private def shutdown(cmd: ShutDown): IO[Unit] =
    logger.debugIO:
      locally:
        // Run asynchronously as a fiber:
        if cmd.isFailover then
          agentMotor.flatMapSome: agentMotor =>
            agentMotor.kill *> stopAgentMotor(cmd)
          .productR:
            journal.kill
        else if cmd.isSwitchover then
          stopAgentMotor(cmd) *>
            workingClusterNode.switchOver.map(_.orThrow)
        else
          stopAgentMotor(cmd)
      .productR:
        stop
      .productR:
        shuttingDown.get.map(_ getOrElse cmd).flatMap: runningShutdown =>
          terminated.complete:
            // If dedicated while being shut down, the dedicated AgentMotor is
            // being stopped asynchronously in background.
            DirectorTermination(
              restartJvm = runningShutdown.restart,
              restartDirector = runningShutdown.restartDirector)
          .void

  private def whenNotShuttingDown[A](body: IO[Checked[A]]): IO[Checked[A]] =
    shuttingDown.get.flatMap:
      case None => body
      case Some(_) => IO.left(AgentIsShuttingDown)

  private def startAgentMotor(agentPath: AgentPath, controllerId: ControllerId): IO[Checked[Unit]] =
    lock.lock:
      whenNotShuttingDown:
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
    agentConf: AgentConfiguration,
    actorSystem: ActorSystem)
  : ResourceIO[AgentCommandExecutor] =
    Service:
      for
        failedOverSubagentId <-
          workingClusterNode.journal.aggregate.map: agentState =>
            workingClusterNode.failedNodeId.map: nodeId =>
              agentState.meta.clusterNodeIdToSubagentId(nodeId).orThrow
        shuttingDownAtomic <- AtomicCell[IO].of(none[ShutDown])
      yield
        new AgentCommandExecutor(forDirector, failedOverSubagentId, workingClusterNode,
          agentConf, actorSystem,
          shuttingDownAtomic)


  private final case class Dedicated(
    private val allocated: Allocated[IO, AgentMotor]):

    def agentMotor = allocated.allocatedThing

    def stopAgentMotor = allocated.release
