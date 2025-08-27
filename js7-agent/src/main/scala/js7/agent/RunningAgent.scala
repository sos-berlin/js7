package js7.agent

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO, Resource, ResourceIO}
import cats.syntax.all.*
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
import js7.base.catsutils.CatsEffectExtensions.{left, right, startAndForget}
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.catsutils.{Environment, OurIORuntimeRegister}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.eventbus.StandardEventBus
import js7.base.generic.SecretString
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.service.{MainService, Service}
import js7.base.system.startup.StartUp
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.{AlarmClock, WallClock}
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, Atomic, ProgramTermination}
import js7.base.web.Uri
import js7.cluster.ClusterNode
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.core.command.CommandMeta
import js7.core.license.LicenseChecker
import js7.data.Problems.PassiveClusterNodeShutdownNotAllowedProblem
import js7.data.node.NodeNameToPassword
import js7.data.state.EngineStateMXBean
import js7.journal.EventIdGenerator
import js7.journal.files.JournalFiles.extensions.*
import js7.journal.watch.StrictEventWatch
import js7.license.LicenseCheckContext
import js7.subagent.Subagent
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.directives.SecurityDirectives.Authenticator
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

  val subagent: Subagent = forDirector.subagent
  val localUri: Uri = subagent.localUri
  val eventWatch: StrictEventWatch = clusterNode.recoveredExtract.eventWatch.strict

  private val shutdownBeforeClusterNodeActivated = Deferred.unsafe[IO, (ShutDown, CommandMeta)]
  private val agentCommandExecutorDeferred =
    Deferred.unsafe[IO, Checked[Allocated[IO, AgentCommandExecutor]]]
  private val isTerminating = Atomic(false)

  // SubagentCommand.ShutDown command shuts down the director, too.
  // Then the restart flag of the command should be respected
  @volatile private var subagentTermination = ProgramTermination()

  val untilReady: IO[Checked[AgentCommandExecutor]] =
    agentCommandExecutorDeferred.get.map(_.map(_.allocatedThing))

  val untilTerminated: IO[Termination] =
    memoize:
      agentCommandExecutorDeferred.get.flatMap:
        case Left(problem) =>
          logger.debug(s"❓ untilTerminated: $problem")
          IO.pure(DirectorTermination(/*???*/))
        case Right(Allocated(agentCommandExecutor, _)) =>
          agentCommandExecutor.untilTerminated.map: termination =>
            termination.copy(
              restartJvm = termination.restartJvm | subagentTermination.restart)
      .guarantee(IO:
        isTerminating := true)

  def agentState: IO[Checked[AgentState]] =
    clusterNode.currentState

  protected def start =
    logger.debugIO("untilActivated handling"):
      clusterNode.untilActivated.flatMap:
        case Left(termination) =>
          agentCommandExecutorDeferred.complete(Left(DirectorTerminatedProblem(termination)))
        case Right(workingClusterNode) =>
          AgentCommandExecutor.service(forDirector, workingClusterNode, clock, conf, actorSystem)
            .toAllocated
            .flatTap: allocated =>
              agentCommandExecutorDeferred.complete(Right(allocated)).flatMap:
                IO.unlessA(_):
                  allocated.release
              .productR:
                shutdownBeforeClusterNodeActivated.get.flatMap: (cmd, meta) =>
                  allocated.allocatedThing.execute(cmd, meta)
      .race(untilStopped)
    .startAndForget
    .productR:
      startService:
        forDirector.subagent.untilTerminated
          .flatTap(termination => IO:
            subagentTermination = termination)
          .*>(stop/*TODO Subagent should terminate this Director ?*/)
          .startAndForget
          .*>(IO.race(
            untilStopRequested *> stopMe,
            untilTerminated))
          .void

  private def stopMe: IO[Unit] =
    logger.debugIO:
      terminating:
        executeCommandAsSystemUser(ShutDown(Some(SIGTERM)))
          .attempt
          .map:
            case Left(throwable) => logger.warn(throwable.toStringWithCauses)
            case Right(Left(problem)) => logger.warn(problem.toString)
            case Right(Right(_)) =>
          .startAndForget
      .void

  private[agent] def terminate(
    processSignal: Option[ProcessSignal] = None,
    clusterAction: Option[ShutDown.ClusterAction] = None,
    suppressSnapshot: Boolean = false,
    restartDirector: Boolean = false)
  : IO[DirectorTermination] =
    logger.debugIO:
      terminating:
        executeCommand(
          ShutDown(
            processSignal, clusterAction,
            suppressSnapshot = suppressSnapshot,
            restartDirector = restartDirector),
          CommandMeta(SimpleUser.System)
        ).map(_.orThrow)

  private def terminating(body: IO[Unit]): IO[DirectorTermination] =
    IO.defer:
      IO.unlessA(isTerminating.getAndSet(true)):
        body
      *> untilTerminated
    .logWhenMethodTakesLonger

  private[agent] def executeCommandAsSystemUser(command: AgentCommand)
  : IO[Checked[AgentCommand.Response]] =
    for
      checkedSession <- forDirector.sessionRegister.systemSession
      checkedChecked <- checkedSession.traverse: session =>
        executeCommand(command, CommandMeta(session.currentUser))
    yield
      checkedChecked.flatten

  // May be called before RunningAgent service has been started
  def executeCommand(cmd: AgentCommand, meta: CommandMeta): IO[Checked[AgentCommand.Response]] =
    logger.debugIO(s"executeCommand ${cmd.getClass.shortClassName}"):
      cmd.match
        case cmd: AgentCommand.ShutDown => // TODO ShutDown must not be in a Batch command
          if cmd.clusterAction.nonEmpty && !clusterNode.isWorkingNode then
            IO.left(PassiveClusterNodeShutdownNotAllowedProblem)
          else
            IO.defer:
              logger.info(s"❗ $cmd")
              val termination = DirectorTermination(
                restartJvm = cmd.restart,
                restartDirector = cmd.restartDirector)
              // Notify ClusterNode in case it sticks in ClusterWatchCounterPart
              clusterNode.announceShutdown(termination) *>
                agentCommandExecutorDeferred.tryGet.flatMap:
                  case None =>
                    shutdownBeforeClusterNodeActivated.complete(cmd -> meta).map: offered =>
                      if offered then
                        Right(AgentCommand.Response.Accepted)
                      else
                        Left(Problem("Starting Director is already shutting down"))

                  case Some(Left(problem)) =>
                    logger.debug(s"⚠️  $cmd, but agentCommandExecutorDeferred = $problem")
                    IO.right(AgentCommand.Response.Accepted)
                    //IO.left(problem)

                  case Some(Right(Allocated(o, _))) =>
                    o.execute(cmd, meta)
        case _ =>
          agentCommandExecutorDeferred.get.flatMapT:
            _.allocatedThing.execute(cmd, meta)
      .logWhenItTakesLonger(s"${cmd.getClass.simpleScalaName} command")

  def systemSessionToken: SessionToken =
    forDirector.systemSessionToken

  override def toString = "RunningAgent"


object RunningAgent:
  private val logger = Logger[this.type]

  def resource(
    conf: AgentConfiguration,
    testWiring: TestWiring = TestWiring.empty)
    (using ioRuntime: IORuntime)
  : ResourceIO[RunningAgent] =
    for
      subagent <- subagentResource(conf)
      director <- director(subagent, conf, testWiring)
    yield
      director

  def restartable(
    conf: AgentConfiguration,
    testWiring: TestWiring = TestWiring.empty)
    (using ioRuntime: IORuntime)
  : ResourceIO[RestartableDirector] =
    for
      subagent <- subagentResource(conf)
      director <- RestartableDirector.service(subagent, conf, testWiring)
    yield
      director

  private def subagentResource(conf: AgentConfiguration)(using ioRuntime: IORuntime)
  : ResourceIO[Subagent] =
    Resource.suspend:
      IO:
        val testEventBus = new StandardEventBus[Any]
        for
          _ <- Resource.eval(IO:
            if !StartUp.isMain then logger.debug("JS7 Agent starting ...\n" + "┈" * 80)
            conf.createDirectories()
            conf.journalLocation.deleteJournalIfMarked().orThrow)
          subagent <- Subagent.service(conf.subagentConf, testEventBus)
        yield
          subagent
    .evalOn(ioRuntime.compute)

  def director(
    subagent: Subagent,
    conf: AgentConfiguration,
    testWiring: TestWiring = TestWiring.empty)
    (using ioRuntime: IORuntime)
  : ResourceIO[RunningAgent] =
    Resource.suspend(IO:
      import conf.{clusterConf, config, httpsConfig, implicitPekkoAskTimeout, journalLocation}
      val licenseChecker = new LicenseChecker(LicenseCheckContext(conf.configDirectory))
      // TODO Subagent itself should start Director when requested
      val forDirector = subagent.forDirector
      val clock = testWiring.alarmClock getOrElse:
        AlarmClock(
          Some(config.getDuration("js7.time.clock-setting-check-interval").toFiniteDuration)
        )(using ioRuntime.scheduler)
      val eventIdGenerator = testWiring.eventIdGenerator getOrElse EventIdGenerator(clock)
      implicit val nodeNameToPassword: NodeNameToPassword[AgentState] =
        nodeName =>
          Right(config.optionAs[SecretString]:
            "js7.auth.subagents." + ConfigUtil.joinPath(nodeName.string))

      val env = OurIORuntimeRegister.toEnvironment(ioRuntime)
      for
        _ <- env.registerPure[IO, AlarmClock](clock, ignoreDuplicate = true)
        _ <- env.registerPure[IO, WallClock](clock, ignoreDuplicate = true)
        _ <- env.registerPure[IO, EventIdGenerator](eventIdGenerator, ignoreDuplicate = true)
        //_ <- Environment.registerPure(testEventBus.narrowPublisher[Stamped[AnyKeyedEvent]])
        clusterNode <- ClusterNode.recoveringResource[AgentState](
          pekkoResource = Resource.eval(IO.pure(forDirector.actorSystem)),
          (admission, label, actorSystem) => AgentClient.resource(
            admission, label, httpsConfig)(using actorSystem),
          licenseChecker,
          journalLocation, clusterConf, eventIdGenerator, subagent.testEventBus)
        director <-
          resource2(forDirector, clusterNode, subagent.testEventBus, conf, clock)
      yield
        director)

  private def resource2(
    forDirector: Subagent.ForDirector,
    clusterNode: ClusterNode[AgentState],
    testEventBus: StandardEventBus[Any],
    conf: AgentConfiguration,
    clock: AlarmClock)
    (using x: IORuntime)
  : ResourceIO[RunningAgent] =
    import clusterNode.actorSystem

    // ClusterNode is running, maybe awaiting activation
    // Return RunningAgent, even if cluster node has not yet activated

    for
      nonStartedAgent <- Resource.eval(IO:
        RunningAgent(forDirector, clusterNode, testEventBus, conf, clock, actorSystem))
      _ <- forDirector.subagent.registerDirectorRoute:
        routeBinding => IO:
          AgentRoute(
            routeBinding, nonStartedAgent.executeCommand, clusterNode,
            conf,
            GateKeeper.Configuration.fromConfig(conf.config, SimpleUser.apply,
              // AgentDirectorPermission is not used for local Subagent.
              // But the user may have a AgentDirectorPermission, in case the Subagent
              // will become remote after a switchover or failover.
              // See SubagentWebServer.
              Set(AgentDirectorPermission)),
            forDirector.sessionRegister
          ).agentRoute
      _ <- EngineStateMXBean.register
      agent <- Service.resource(nonStartedAgent)
    yield
      agent


  final case class TestWiring(
    alarmClock: Option[AlarmClock] = None,
    eventIdGenerator: Option[EventIdGenerator] = None,
    commandHandler: Option[CommandHandler] = None,
    authenticator: Option[AgentConfiguration => Authenticator[SimpleUser]] = None,
    envResources: Seq[Environment.TaggedResource[IO, ?]] = Nil)

  object TestWiring:
    val empty: TestWiring = TestWiring()


  private[agent] final class DirectorTerminatedException(val termination: ProgramTermination)
  extends RuntimeException

  private[agent] final case class DirectorTerminatedProblem(termination: ProgramTermination)
  extends Problem.Coded:
    def arguments = Map1("termination", termination.toString)
