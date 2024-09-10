package js7.agent

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO, Resource, ResourceIO}
import cats.syntax.all.*
import com.softwaremill.tagging.{@@, Tagger}
import com.typesafe.config.ConfigUtil
import js7.agent.RunningAgent.*
import js7.agent.client.AgentClient
import js7.agent.command.CommandHandler
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.agent.data.views.AgentOverview
import js7.agent.web.AgentRoute
import js7.base.BuildInfo
import js7.base.auth.{SessionToken, SimpleUser}
import js7.base.catsutils.CatsEffectExtensions.{left, right, startAndForget}
import js7.base.catsutils.Environment
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.eventbus.StandardEventBus
import js7.base.generic.SecretString
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.tapError
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.problem.Problems.ShuttingDownProblem
import js7.base.service.{MainService, Service}
import js7.base.system.SystemInformations.systemInformation
import js7.base.system.startup.StartUp
import js7.base.time.AlarmClock
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, Atomic, ProgramTermination}
import js7.base.web.Uri
import js7.cluster.ClusterNode
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.system.JavaInformations.javaInformation
import js7.core.command.CommandMeta
import js7.core.license.LicenseChecker
import js7.data.Problems.{BackupClusterNodeNotAppointed, ClusterNodeIsNotActiveProblem, ClusterNodeIsNotReadyProblem, PassiveClusterNodeShutdownNotAllowedProblem}
import js7.data.node.{NodeId, NodeNameToPassword}
import js7.data.subagent.SubagentId
import js7.journal.EventIdGenerator
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.state.FileJournal
import js7.journal.watch.JournalEventWatch
import js7.license.LicenseCheckContext
import js7.subagent.Subagent
import org.apache.pekko.actor.{ActorRef, ActorSystem, Props}
import org.apache.pekko.http.scaladsl.server.directives.SecurityDirectives.Authenticator
import scala.collection.mutable
import scala.concurrent.{Future, Promise}

final class RunningAgent private(
  clusterNode: ClusterNode[AgentState],
  val journal: IO[FileJournal[AgentState]],
  getLocalUri: () => Uri,
  untilMainActorTerminated: IO[DirectorTermination],
  val untilReady: IO[MainActor.Ready],
  val executeCommand: (AgentCommand, CommandMeta) => IO[Checked[AgentCommand.Response]],
  forDirector: Subagent.ForDirector,
  val testEventBus: StandardEventBus[Any],
  val actorSystem: ActorSystem,
  val conf: AgentConfiguration)
  (using val ioRuntime: IORuntime)
extends MainService, Service.StoppableByRequest:

  protected type Termination = DirectorTermination

  lazy val localUri: Uri = getLocalUri()
  val eventWatch: JournalEventWatch = clusterNode.recoveredExtract.eventWatch
  val subagent: Subagent = forDirector.subagent
  private val isTerminating = Atomic(false)

  // SubagentCommand.ShutDown command shuts down the director, too.
  // Then the restart flag of the command should be respected
  @volatile private var subagentTermination = ProgramTermination()

  val untilTerminated: IO[Termination] =
    memoize:
      untilMainActorTerminated
        .map(termination => termination.copy(
          restartJvm = termination.restartJvm | subagentTermination.restart))
        .guarantee(IO(isTerminating := true))

  def agentState: IO[Checked[AgentState]] =
    clusterNode.currentState

  protected def start =
    startService:
      forDirector.subagent.untilTerminated
        .flatTap(termination => IO:
          subagentTermination = termination)
        .*>(stop/*TODO Subagent should terminate this Director ?*/)
        .startAndForget
        .*>(IO.race(
          untilStopRequested *> shutdown,
          untilTerminated))
        .void

  private def shutdown: IO[Unit] =
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
    suppressSnapshot: Boolean = false)
  : IO[DirectorTermination] =
    logger.debugIO:
      terminating:
        executeCommand(
          ShutDown(processSignal, clusterAction, suppressSnapshot = suppressSnapshot),
          CommandMeta(SimpleUser.System)
        ).map(_.orThrow)
    .evalOn(ioRuntime.compute) // Only for TestAgent

  private def terminating(body: IO[Unit]): IO[DirectorTermination] =
    IO.defer:
      IO
        .unlessA(isTerminating.getAndSet(true)):
          body
        .*>(untilTerminated)
        .logWhenItTakesLonger

  private[agent] def executeCommandAsSystemUser(command: AgentCommand)
  : IO[Checked[AgentCommand.Response]] =
    for
      checkedSession <- forDirector.sessionRegister.systemSession
      checkedChecked <- checkedSession.traverse: session =>
        executeCommand(command, CommandMeta(session.currentUser))
    yield
      checkedChecked.flatten

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
      director <- RestartableDirector(subagent, conf, testWiring)
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
          subagent <- Subagent.resource(conf.subagentConf, testEventBus)
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

      for
        clusterNode <- ClusterNode.recoveringResource[AgentState](
          pekkoResource = Resource.eval(IO.pure(forDirector.actorSystem)),
          (admission, label, actorSystem) => AgentClient.resource(
            admission, label, httpsConfig)(actorSystem),
          licenseChecker,
          journalLocation, clusterConf, eventIdGenerator, subagent.testEventBus)
        director <-
          resource2(forDirector, clusterNode, testWiring, conf, subagent.testEventBus, clock)
      yield
        director)

  private def resource2(
    forDirector: Subagent.ForDirector,
    clusterNode: ClusterNode[AgentState],
    testWiring: TestWiring,
    conf: AgentConfiguration,
    testEventBus: StandardEventBus[Any],
    clock: AlarmClock)
    (using ioRuntime: IORuntime)
  : ResourceIO[RunningAgent] =
    import clusterNode.actorSystem
    import conf.config

    val actors = mutable.Buffer.empty[ActorRef]

    def startMainActor(
      failedNodeId: Option[NodeId],
      journalAllocated: Allocated[IO, FileJournal[AgentState]])
    : MainActorStarted =
      val failedOverSubagentId: Option[SubagentId] =
        for nodeId <- failedNodeId yield
          journalAllocated.allocatedThing.unsafeCurrentState().meta
            .clusterNodeIdToSubagentId(nodeId)
            .orThrow

      val mainActorReadyPromise = Promise[MainActor.Ready]()
      val terminationPromise = Promise[DirectorTermination]()
      val actor = actorSystem.actorOf(
        Props {
          val mainActor = new MainActor(
            forDirector,
            failedOverSubagentId,
            clusterNode,
            journalAllocated, conf, testWiring.commandHandler,
            mainActorReadyPromise, terminationPromise,
            clock)
            (using ioRuntime)
          for o <- mainActor.commandActor do actors += o
          mainActor
        },

        "main").taggedWith[MainActor]

      actors += actor
      actor ! MainActor.Input.Start(journalAllocated.allocatedThing.unsafeCurrentState())

      MainActorStarted(
        actor,
        IO.fromFuture(IO(mainActorReadyPromise.future)),
        terminationPromise.future)

    val journalDeferred = Deferred.unsafe[IO, FileJournal[AgentState]]

    val mainActorStarted: IO[Either[DirectorTermination, MainActorStarted]] =
      memoize:
        logger.traceIOWithResult:
          clusterNode.untilActivated
            .map(_.left.map(DirectorTermination.fromProgramTermination))
            .flatMapT: workingClusterNode =>
              journalDeferred.complete(workingClusterNode.journalAllocated.allocatedThing)
                .*>(IO:
                  startMainActor(
                    workingClusterNode.failedNodeId,
                    workingClusterNode.journalAllocated))
                .map(Right(_))
            //.onErrorRecover { case t: RestartAfterJournalTruncationException =>
            //  logger.info(t.getMessage)
            //  Left(t.termination)
            //}

    @deprecated val whenReady = Promise[Unit] // NOT USED ?

    val untilReady: IO[MainActor.Ready] =
      mainActorStarted.flatMap:
        case Left(_: DirectorTermination) => IO.raiseError(IllegalStateException:
          "Agent has been terminated")
        case Right(mainActorStarted) => mainActorStarted.whenReady

    // The AgentOrderKeeper if started
    val currentMainActor: IO[Checked[MainActorStarted]] =
      clusterNode.currentState
        .map(_.map(_.clusterState))
        .flatMapT: clusterState =>
          import clusterNode.clusterConf.{isBackup, ownId}
          if !clusterState.isActive(ownId, isBackup = isBackup) then
            IO.left(ClusterNodeIsNotActiveProblem)
          else
            mainActorStarted.map:
              case Left(_) => Left(ShuttingDownProblem)
              case Right(o) => Right(o)
        .tapError(t => IO:
          logger.debug(s"currentOrderKeeperActor => ${t.toStringWithCauses}", t)
          whenReady.tryFailure(t))

    val untilMainActorTerminated =
      memoize:
        logger
          .traceIO:
            mainActorStarted
              .flatMap:
                case Left(termination) => IO.pure(termination)
                case Right(o) =>
                  IO
                    .fromFuture(IO(o.termination))
                    .tapError(t => IO:
                      logger.error(s"MainActor failed with ${t.toStringWithCauses}", t))
              .tapError(t => IO(whenReady.tryFailure(t)))
          .uncancelable /*a test may use this in `race`, unintentionally canceling this*/

    val gateKeeperConf = GateKeeper.Configuration.fromConfig(config, SimpleUser.apply)

    val agentOverview = IO(AgentOverview(
      startedAt = StartUp.startedAt,
      version = BuildInfo.prettyVersion,
      buildId = BuildInfo.buildId,
      //isTerminating = isTerminating,
      system = systemInformation(),
      java = javaInformation()))

    def executeCommand(cmd: AgentCommand, meta: CommandMeta)
    : IO[Checked[AgentCommand.Response]] =
      logger.debugIO(s"executeCommand ${cmd.getClass.shortClassName}")(cmd
        .match
          case cmd: ShutDown =>
            if cmd.clusterAction.nonEmpty && !clusterNode.isWorkingNode then
              IO.left(PassiveClusterNodeShutdownNotAllowedProblem)
            else IO.defer:
              logger.info(s"❗ $cmd")
              //⚒️if (cmd.dontNotifyActiveNode && clusterNode.isPassive) {
              //⚒️  clusterNode.dontNotifyActiveNodeAboutShutdown()
              //⚒️}
              clusterNode
                .stopRecovery(DirectorTermination(
                  restartJvm = cmd.restart,
                  restartDirector = cmd.restartDirector)
                ) >>
                currentMainActor
                  .flatMap(_.traverse(_.whenReady.map(_.api)))
                  .flatMap:
                    case Left(problem @ (ClusterNodeIsNotActiveProblem | ShuttingDownProblem
                              | BackupClusterNodeNotAppointed)) =>
                      logger.debug(s"❓$problem")
                      IO.right(AgentCommand.Response.Accepted)

                    case Left(problem @ ClusterNodeIsNotReadyProblem /*???*/) =>
                      logger.error(s"❓ $cmd => $problem")
                      IO.right(AgentCommand.Response.Accepted)

                    case Left(problem) =>
                      IO.left(problem)

                    case Right(api) =>
                      api(meta).commandExecute(cmd)

          case _ =>
            currentMainActor
              .flatMap(_.traverse(_.whenReady.map(_.api)))
              .flatMapT(api => api(meta).commandExecute(cmd))
        .map(_.map((_: AgentCommand.Response).asInstanceOf[cmd.Response]))
        .logWhenItTakesLonger(s"${cmd.getClass.simpleScalaName} command"))

    for
      _ <- Resource.onFinalize(IO:
        for actor <- actors do actorSystem.stop(actor))
      agent <- Service.resource(IO:
        new RunningAgent(
          clusterNode,
          journalDeferred.get,
          () => forDirector.subagent.localUri,
          untilMainActorTerminated,
          untilReady, executeCommand,
          forDirector,
          testEventBus,
          actorSystem, conf))
      _ <- forDirector.subagent.directorRegisteringResource:
        routeBinding => IO.pure:
          new AgentRoute(
            agentOverview,
            routeBinding,
            agent.executeCommand,
            clusterNode,
            conf,
            gateKeeperConf,
            forDirector.sessionRegister
          ).agentRoute
    yield
      agent
  end resource2


  final case class TestWiring(
    alarmClock: Option[AlarmClock] = None,
    eventIdGenerator: Option[EventIdGenerator] = None,
    commandHandler: Option[CommandHandler] = None,
    authenticator: Option[AgentConfiguration => Authenticator[SimpleUser]] = None,
    envResources: Seq[Environment.TaggedResource[IO, ?]] = Nil)

  object TestWiring:
    val empty: TestWiring = TestWiring()


  private case class MainActorStarted(
    actor: ActorRef @@ MainActor,
    whenReady: IO[MainActor.Ready],
    termination: Future[DirectorTermination])
