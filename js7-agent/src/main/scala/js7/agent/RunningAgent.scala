package js7.agent

import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.effect.{Deferred, IO, Resource, ResourceIO}
import com.typesafe.config.ConfigUtil
import js7.agent.RunningAgent.*
import js7.agent.client.AgentClient
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.agent.motor.AgentCommandExecutor
import js7.agent.web.AgentRoute
import js7.base.auth.{AgentDirectorPermission, SessionToken, SimpleUser}
import js7.base.catsutils.CatsEffectExtensions.{left, right}
import js7.base.catsutils.{Environment, OurIORuntimeRegister}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.eventbus.StandardEventBus
import js7.base.generic.SecretString
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.service.{MainService, Service}
import js7.base.system.startup.StartUp
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.{AlarmClock, WallClock}
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.:=
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.cluster.ClusterNode
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.core.command.CommandMeta
import js7.core.license.LicenseChecker
import js7.data.Problems.PassiveClusterNodeShutdownNotAllowedProblem
import js7.data.agent.Problems.AgentIsShuttingDown
import js7.data.node.NodeNameToPassword
import js7.data.state.EngineStateMXBean
import js7.data.subagent.Problems.NoDirectorProblem
import js7.journal.EventIdGenerator
import js7.journal.files.JournalFiles.extensions.*
import js7.journal.watch.StrictEventWatch
import js7.license.LicenseCheckContext
import js7.subagent.Subagent
import org.apache.pekko.actor.ActorSystem
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Map.Map1

final class RunningAgent private(
  forDirector: Subagent.ForDirector,
  clusterNode: ClusterNode[AgentState],
  val testEventBus: StandardEventBus[Any],
  val conf: AgentConfiguration,
  val clock: AlarmClock,
  val actorSystem: ActorSystem)
  (using val ioRuntime: IORuntime)
extends MainService, Service.StoppableByRequest:

  protected type Termination = DirectorTermination

  val localSubagent: Subagent = forDirector.subagent
  val localUri: Uri = localSubagent.localUri
  @TestOnly
  val eventWatch: StrictEventWatch = clusterNode.recoveredExtract.eventWatch.strict

  private val shutdownBeforeClusterNodeActivated = Deferred.unsafe[IO, (ShutDown, CommandMeta)]
  private val agentCommandExecutorDeferred =
    Deferred.unsafe[IO, Either[DirectorTerminatedProblem, AgentCommandExecutor]]
  private val whenTerminated = Deferred.unsafe[IO, DirectorTermination]
  private val isTerminating = Atomic(false)

  protected def start =
    startService:
      clusterNode.untilActivated.flatMap:
        case Left(termination) =>
          val directorTermination = DirectorTermination.fromProgramTermination(termination)
          agentCommandExecutorDeferred.complete:
            Left(DirectorTerminatedProblem(directorTermination))
          .productR:
            whenTerminated.complete(directorTermination)
          .void

        case Right(workingClusterNode) =>
          AgentCommandExecutor.service(forDirector, workingClusterNode, conf, actorSystem)
            .use: agentCommandExecutor =>
              agentCommandExecutorDeferred.complete(Right(agentCommandExecutor))
                .productR:
                  shutdownBeforeClusterNodeActivated.get.flatMap: (cmd, meta) =>
                    agentCommandExecutor.execute(cmd, meta)
                      .handleProblem: problem =>
                        logger.error(s"shutdownBeforeClusterNodeActivated $cmd: $problem")
                  .background.surround:
                    agentCommandExecutor.untilTerminated
                .flatTap: termination =>
                  isTerminating := true
                  whenTerminated.complete(termination)
                .productR:
                  localSubagent.untilTerminated
                    .flatMap: termination =>
                      logger.debug("Subagent has terminated")
                      terminate(restart = termination.restart)
                .background.surround:
                  (untilStopRequested *> shutdownDueToStop)
                    .background.surround:
                      untilTerminated.void
      .onError: t =>
        agentCommandExecutorDeferred.complete:
          Left(DirectorTerminatedProblem(DirectorTermination()))
        .void

  @TestOnly
  private[agent] def untilReady: IO[Checked[AgentCommandExecutor]] =
    agentCommandExecutorDeferred.get

  private def shutdownDueToStop: IO[Unit] =
    logger.debugIO:
      terminating:
        executeCommand(ShutDown(), CommandMeta.system("RunningAgent shutdownDueToStop"))
          .attempt.map:
            case Left(t) =>
              isTerminating := false
              logger.warn(s"Shutdown: ${t.toStringWithCauses}", t)
            case Right(Left(problem @ AgentIsShuttingDown)) =>
              logger.debug(s"Shutdown: $problem}")
            case Right(Left(problem)) =>
              isTerminating := false
              logger.warn(s"Shutdown: $problem}")
            case Right(Right(_)) =>
      .void

  private[agent] def terminate(
    processSignal: Option[ProcessSignal] = None,
    clusterAction: Option[ShutDown.ClusterAction] = None,
    suppressSnapshot: Boolean = false,
    restart: Boolean = false,
    restartDirector: Boolean = false)
  : IO[DirectorTermination] =
    logger.debugIO:
      terminating:
        executeCommand(
          ShutDown(
            processSignal, clusterAction,
            suppressSnapshot = suppressSnapshot,
            restart = restart,
            restartDirector = restartDirector),
          CommandMeta.system("RunningAgent.terminate")
        ).map(_.orThrow)

  private def terminating(body: IO[Unit]): IO[DirectorTermination] =
    IO.defer:
      IO.unlessA(isTerminating.getAndSet(true)):
        body
      *> untilTerminated
    .logWhenMethodTakesLonger

  def untilTerminated: IO[Termination] =
    logger.traceIOWithResult:
      whenTerminated.get

  // May be called before RunningAgent service has been started
  def executeCommand(cmd: AgentCommand, meta: CommandMeta): IO[Checked[AgentCommand.Response]] =
    logger.debugIO(s"executeCommand ${cmd.getClass.shortClassName}"):
      cmd.match
        case cmd: AgentCommand.ShutDown => // TODO ShutDown must not be in a Batch command
          executeShutdown(cmd, meta)

        case _ =>
          agentCommandExecutorDeferred.tryGet.flatMap:
            case None => IO.left(NoDirectorProblem)
            case Some(Left(problem)) => IO.left(problem)
            case Some(Right(agentCommandExecutor)) =>
              agentCommandExecutor.execute(cmd, meta)

  private def executeShutdown(cmd: ShutDown, meta: CommandMeta): IO[Checked[AgentCommand.Response]] =
    if cmd.clusterAction.nonEmpty && !clusterNode.isWorkingNode then
      IO.left(PassiveClusterNodeShutdownNotAllowedProblem)
    else
      IO.defer:
        logger.info(s"❗️ $cmd • $meta")
        val termination = DirectorTermination(
          restartJvm = cmd.restart,
          restartDirector = cmd.restartDirector)
        // Notify ClusterNode in case it sticks in ClusterWatchCounterPart
        clusterNode.announceShutdown(termination) *>
          agentCommandExecutorDeferred.tryGet.flatMap:
            case None =>
              shutdownBeforeClusterNodeActivated.complete(cmd -> meta).map: ok =>
                if ok then
                  Right(AgentCommand.Response.Accepted)
                else
                  Left(Problem("Starting Director is already shutting down"))

            case Some(Left(problem)) =>
              logger.debug(s"⚠️  $cmd, but agentCommandExecutorDeferred = $problem")
              IO.right(AgentCommand.Response.Accepted)
              //IO.left(problem)

            case Some(Right(o)) =>
              o.execute(cmd, meta)
      .logWhenItTakesLonger(s"${cmd.getClass.simpleScalaName} command")

  def agentState: IO[Checked[AgentState]] =
    clusterNode.currentState

  def systemSessionToken: SessionToken =
    forDirector.systemSessionToken

  override def toString = "RunningAgent"


object RunningAgent:
  private val logger = Logger[this.type]

  def withSubagent(
    conf: AgentConfiguration,
    testWiring: TestWiring = TestWiring.empty,
    subagentTestWiring: Subagent.TestWiring = Subagent.TestWiring.empty)
    (using ioRuntime: IORuntime)
  : ResourceIO[RunningAgent] =
    for
      subagent <- subagentService(conf, subagentTestWiring)
      director <- service(subagent, conf, testWiring)
    yield
      director

  def restartableDirector(
    conf: AgentConfiguration,
    testWiring: TestWiring = TestWiring.empty,
    subagentTestWiring: Subagent.TestWiring = Subagent.TestWiring.empty)
    (using ioRuntime: IORuntime)
  : ResourceIO[RestartableDirector] =
    for
      subagent <- subagentService(conf, subagentTestWiring)
      director <- RestartableDirector.service(subagent, conf, testWiring)
    yield
      director

  private def subagentService(conf: AgentConfiguration, testWiring: Subagent.TestWiring)
    (using ioRuntime: IORuntime)
  : ResourceIO[Subagent] =
    Resource.suspend:
      IO:
        if !StartUp.isMain then logger.debug("JS7 Agent Director is starting ...\n" + "┈" * 80)
        conf.createDirectories()
        conf.journalLocation.deleteJournalIfMarked().orThrow
        Subagent.service(conf.subagentConf, testWiring)
    .evalOn(ioRuntime.compute)

  def service(
    subagent: Subagent,
    conf: AgentConfiguration,
    testWiring: TestWiring = TestWiring.empty)
    (using ioRuntime: IORuntime)
  : ResourceIO[RunningAgent] =
    Resource.suspend(IO:
      import conf.{clusterConf, config, httpsConfig, journalLocation}
      val licenseChecker = new LicenseChecker(LicenseCheckContext(conf.configDirectory))
      // TODO Subagent itself should start Director when requested
      val forDirector = subagent.forDirector
      given NodeNameToPassword[AgentState] =
        nodeName =>
          Right(config.optionAs[SecretString]:
            "js7.auth.subagents." + ConfigUtil.joinPath(nodeName.string))

      val env = OurIORuntimeRegister.toEnvironment(ioRuntime)
      for
        clock <- Environment.getOrRegister[AlarmClock]:
          given Scheduler = ioRuntime.scheduler
          AlarmClock.resource(
            Some(config.getDuration("js7.time.clock-setting-check-interval").toFiniteDuration))
        _ <- Environment.getOrRegister[WallClock](Resource.pure(clock))
        eventIdGenerator = testWiring.eventIdGenerator getOrElse EventIdGenerator(clock)
        _ <- env.registerPure[IO, EventIdGenerator](eventIdGenerator, ignoreDuplicate = true)
        clusterNode <- ClusterNode.recoveringResource[AgentState](
          pekkoResource = Resource.eval(IO.pure(forDirector.actorSystem)),
          clusterNodeApi = (admission, label, actorSystem) =>
            AgentClient.resource(admission, label, httpsConfig)(using actorSystem),
          licenseChecker,
          journalLocation, clusterConf, eventIdGenerator, subagent.testEventBus)
        director <- service2(forDirector, clusterNode, subagent.testEventBus, conf, clock)
      yield
        director)

  private def service2(
    forDirector: Subagent.ForDirector,
    clusterNode: ClusterNode[AgentState],
    testEventBus: StandardEventBus[Any],
    conf: AgentConfiguration,
    clock: AlarmClock)
    (using IORuntime)
  : ResourceIO[RunningAgent] =
    import clusterNode.actorSystem

    // ClusterNode is running, maybe awaiting activation
    // Return RunningAgent, even if cluster node has not yet activated
    for
      _ <- EngineStateMXBean.register
      runningAgent <- Resource.eval(IO:
        RunningAgent(forDirector, clusterNode, testEventBus, conf, clock, actorSystem))
      _ <- forDirector.subagent.registerDirectorRoute:
        routeBinding => IO:
          AgentRoute(
            routeBinding, runningAgent.executeCommand, clusterNode,
            conf,
            GateKeeper.Configuration.fromConfig(conf.config, SimpleUser.apply,
              // AgentDirectorPermission is not used for local Subagent.
              // But the user may have a AgentDirectorPermission, in case the Subagent
              // will become remote after a switchover or failover.
              // See SubagentWebServer.
              Set(AgentDirectorPermission)),
            forDirector.sessionRegister
          ).agentRoute
      agent <- Service.resource(runningAgent)
    yield
      agent


  final case class TestWiring(
    eventIdGenerator: Option[EventIdGenerator] = None,
    commandHandler: Option[CommandHandler] = None,
    envResources: Seq[Environment.TaggedResource[IO, ?]] = Nil)

  object TestWiring:
    val empty: TestWiring = TestWiring()


  private[RunningAgent] final case class DirectorTerminatedProblem(termination: DirectorTermination)
  extends Problem.Coded:
    def arguments = Map1("termination", termination.toString)
