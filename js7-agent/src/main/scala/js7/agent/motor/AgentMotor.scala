package js7.agent.motor

import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Ref, Resource, ResourceIO}
import java.time.ZoneId
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{IsItemCommand, IsOrderCommand, ResetSubagent}
import js7.agent.data.event.AgentEvent.{AgentReady, AgentShutDown}
import js7.agent.motor.AgentMotor.*
import js7.base.catsutils.CatsEffectExtensions.left
import js7.base.catsutils.Environment.environment
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked.{Ops, RichCheckedF}
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.AlarmClock
import js7.base.utils.Atomic
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.WorkingClusterNode
import js7.common.system.PlatformInfos.currentPlatformInfo
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.event.JournalEvent.JournalEventsReleased
import js7.data.event.{Event, EventId}
import js7.data.subagent.SubagentId
import js7.journal.{FileJournal, Persisted}
import js7.subagent.Subagent
import js7.subagent.director.SubagentKeeper
import org.apache.pekko.actor.ActorSystem

/** The dedicated Director.
  *
  * The Director runs Orders.
  * For Job execution, the Director delegates Orders via the SubagentKeeper to a number of
  * Subagents.
  */
final class AgentMotor private(
  itemCommandExecutor: ItemCommandExecutor,
  orderMotor: OrderMotor,
  val controllerId: ControllerId,
  val agentPath: AgentPath,
  subagentKeeper: SubagentKeeper[AgentState],
  journal: FileJournal[AgentState],
  conf: AgentConfiguration)
extends Service.StoppableByRequest:

  private val agentProcessCount = Atomic(0)
  private val _stopImmediately = Ref.unsafe[IO, Boolean](false)
  private val _shutdown = Ref.unsafe[IO, Option[AgentCommand.ShutDown]](None)
  /** When only the Director should be shut down while the Subagent keeps running. */
  private val _inhibitShutdownLocalSubagent = Ref.unsafe[IO, Boolean](false)

  protected def start =
    journal.aggregate.flatMap: agentState =>
      orderMotor.recoverWorkflows(agentState) *>
        journal.persist:
          AgentReady(
            ZoneId.systemDefault.getId,
            totalRunningTime = journal.totalRunningTime,
            Some(currentPlatformInfo()))
        .map(_.orThrow) *>
        orderMotor.recoverOrders(agentState)
      *>
        startService:
          untilStopRequested *> stopMe

  private def stopMe: IO[Unit] =
    IO.defer:
      orderMotor.stop *>
        _stopImmediately.get.flatMap:
          IO.unlessA(_):
            _inhibitShutdownLocalSubagent.get.flatMap:
              IO.unlessA(_):
                _shutdown.get.map(_.flatMap(_.processSignal)).flatMap: maybeSignal =>
                  subagentKeeper.shutdownLocalSubagent(maybeSignal)
            *>
              journal.persist(AgentShutDown)
                .handleProblemWith: problem =>
                  IO(logger.error(s"AgentShutDown: $problem"))
                .void

  def kill: IO[Unit] =
    logger.debugIO:
      _stopImmediately.set(true) *>
        //subagentKeeper.kill *>
        stop *> subagentKeeper.kill *> journal.kill

  def executeCommand(cmd: AgentCommand): IO[Checked[AgentCommand.Response]] =
    cmd match
      case cmd: IsOrderCommand =>
        orderMotor.executeOrderCommand(cmd)

      case cmd: IsItemCommand  =>
        itemCommandExecutor.executeItemCommand(cmd)

      case ResetSubagent(subagentId, force) =>
        subagentKeeper.startResetSubagent(subagentId, force)
          .rightAs(AgentCommand.Response.Accepted)

      case _ => IO.left(Problem(s"Unknown command: ${cmd.getClass.simpleScalaName}"))

  def resetAllSubagents: IO[Unit] =
    journal.aggregate.flatMap: agentState =>
      subagentKeeper.resetAllSubagents(except = agentState.meta.directors.toSet)

  def shutdown(cmd: AgentCommand.ShutDown): IO[Unit] =
    _shutdown.set(Some(cmd)) *>
      stop

  def inhibitShutdownLocalSubagent: IO[Unit] =
    _inhibitShutdownLocalSubagent.set(true)

  override def toString = "AgentMotor"


object AgentMotor:
  private val logger = Logger[this.type]

  /** AgentMotor with SubagentKeeper (including local Subagent) and FileWatchManager. */
  def service(
    failedOverSubagentId: Option[SubagentId],
    forDirector: Subagent.ForDirector,
    workingClusterNode: WorkingClusterNode[AgentState],
    conf: AgentConfiguration,
    actorSystem: ActorSystem)
  : ResourceIO[AgentMotor] =
    Resource.suspend:
      import forDirector.subagent as localSubagent
      val journal = workingClusterNode.journal
      journal.aggregate.map: agentState =>
        val meta = agentState.meta
        val subagentId: SubagentId =
          if meta.directors.isEmpty then throw new IllegalStateException(
            "Missing definition of Subagents in AgentMetaState")
          else if !conf.clusterConf.isBackup then
            meta.directors.head
          else if meta.directors.sizeIs < 2 then throw new IllegalStateException(
            "Missing definition of backup Subagent in AgentMetaState")
          else
            meta.directors(1)
        (meta.controllerId, meta.agentPath, subagentId)
      .flatMap: (controllerId, ownAgentPath, localSubagentId) =>
        // Automatically add Controller's UserId to the list of users allowed to release events,
        // to avoid deletion of journal files due to an empty list, before controller has read the events.
        // The controller has to send ReleaseEvents commands to release obsolete journal files.
        journal.persist: agentState =>
          val userId = agentState.meta.controllerId.toUserId
          !agentState.journalState.userIdToReleasedEventId.contains(userId) ?
            JournalEventsReleased(userId, EventId.BeforeFirst)
        .map(_.orThrow)
        .map: (_: Persisted[AgentState, Event]) =>
          for
            fileWatchManager <- FileWatchManager.resource(ownAgentPath, journal, conf.config)
            given IORuntime <- Resource.eval(environment[IORuntime])
            given AlarmClock <- Resource.eval(environment[AlarmClock])
            given Dispatcher[IO] <- Dispatcher.parallel[IO]
            subagentKeeper <- SubagentKeeper.resource(
              localSubagentId, localSubagent, ownAgentPath, controllerId, failedOverSubagentId,
              journal, conf.directorConf, actorSystem)
            orderMotor <- OrderMotor.service(ownAgentPath, subagentKeeper, journal, conf)
            itemCommandExecutor = ItemCommandExecutor(forDirector, ownAgentPath, subagentKeeper,
              fileWatchManager, workingClusterNode, conf, orderMotor, journal)
            agentMotor <- Service.resource:
              new AgentMotor(itemCommandExecutor, orderMotor,
                controllerId, ownAgentPath, subagentKeeper, journal, conf)
          yield
            agentMotor
