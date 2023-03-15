package js7.agent

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.server.directives.SecurityDirectives.Authenticator
import cats.effect.Resource
import cats.syntax.traverse.*
import com.softwaremill.diffx.generic.auto.*
import com.softwaremill.tagging.{@@, Tagger}
import com.typesafe.config.Config
import js7.agent.RunningAgent.*
import js7.agent.client.AgentClient
import js7.agent.command.CommandHandler
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{ClusterAppointNodes, ClusterSwitchOver, ShutDown}
import js7.agent.data.views.AgentOverview
import js7.agent.web.AgentWebServer
import js7.agent.web.common.AgentSession
import js7.base.BuildInfo
import js7.base.auth.{SessionToken, SimpleUser, UserId}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.eventbus.StandardEventBus
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.problem.Checked.*
import js7.base.problem.Problems.ShuttingDownProblem
import js7.base.problem.{Checked, Problem}
import js7.base.service.{MainService, Service}
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ProgramTermination}
import js7.base.web.Uri
import js7.cluster.ClusterNode.RestartAfterJournalTruncationException
import js7.cluster.{ClusterConf, ClusterNode}
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.SessionRegister
import js7.common.akkautils.Akkas
import js7.common.system.JavaInformations.javaInformation
import js7.common.system.SystemInformations.systemInformation
import js7.common.system.ThreadPools.newUnlimitedScheduler
import js7.common.system.startup.{ServiceMain, StartUp}
import js7.core.command.CommandMeta
import js7.data.Problems.{BackupClusterNodeNotAppointed, ClusterNodeIsNotActiveProblem, ClusterNodeIsNotReadyProblem}
import js7.journal.EventIdClock
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.recover.Recovered
import js7.journal.state.FileStatePersistence
import js7.journal.watch.EventWatch
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.Atomic
import scala.concurrent.{Future, Promise}

final class RunningAgent private(
  val eventWatch: EventWatch,
  clusterNode: ClusterNode[AgentState],
  webServer: AkkaWebServer & AkkaWebServer.HasUri,
  val terminated: Future[ProgramTermination],
  val untilReady: Task[MainActor.Ready],
  val executeCommand1: (AgentCommand, CommandMeta) => Task[Checked[AgentCommand.Response]],
  sessionRegister: SessionRegister[AgentSession],
  val sessionToken: SessionToken,
  val testEventBus: StandardEventBus[Any],
  val actorSystem: ActorSystem,
  val config: Config)
  (implicit val scheduler: Scheduler)
extends MainService
{
  lazy val localUri: Uri = webServer.localUri

  val untilTerminated: Task[ProgramTermination] =
    Task.fromFuture(terminated)

  def agentState: Task[Checked[AgentState]] =
    clusterNode.currentState

  private val isTerminating = Atomic(false)

  logger.debug("Ready")

  protected def start: Task[Service.Started] =
    startService(untilTerminated.void)

  protected def stop: Task[Unit] =
    terminating(Task {
      executeCommandAsSystemUser(ShutDown(Some(SIGTERM)))
        .runAsyncUncancelable {
          case Left(throwable) => logger.warn(throwable.toStringWithCauses)
          case Right(Left(problem)) => logger.warn(problem.toString)
          case Right(Right(_)) =>
        }
    }).void

  private[agent] def terminate(
    processSignal: Option[ProcessSignal] = None,
    suppressSnapshot: Boolean = false)
  : Task[ProgramTermination] =
    terminating {
      logger.debug("terminate")
      executeCommand(
        AgentCommand.ShutDown(processSignal, suppressSnapshot = suppressSnapshot),
        CommandMeta(SimpleUser.System)
      ).map(_.orThrow)
    }

  private def terminating(body: Task[Unit]): Task[ProgramTermination] =
    Task.defer {
      Task
        .unless(isTerminating.getAndSet(true) || terminated.isCompleted)(
          body)
        .*>(untilTerminated)
    }

  ///** Circumvents the CommandHandler which is possibly replaced by a test via DI. */  // TODO Do we need all this code?
  //private def directExecuteCommand(command: AgentCommand): Task[Checked[AgentCommand.Response]] =
  //  mainActor.flatMap(mainActor =>
  //    Task.deferFuture(
  //      promiseFuture[Checked[AgentCommand.Response]] { promise =>
  //        mainActor !
  //          MainActor.Input.ExternalCommand(command, UserId.Anonymous, CorrelId.current, promise)
  //      }))

  private[agent] def executeCommandAsSystemUser(command: AgentCommand)
  : Task[Checked[AgentCommand.Response]] =
    for {
      checkedSession <- sessionRegister.systemSession
      checkedChecked <- checkedSession.traverse(session =>
        executeCommand(command, CommandMeta(session.currentUser)))
    } yield checkedChecked.flatten

  private[agent] def executeCommand(command: AgentCommand, meta: CommandMeta)
  : Task[Checked[AgentCommand.Response]] =
    logger.debugTask("executeCommand", command.getClass.shortClassName)(
      (command match {
        case command: AgentCommand.ShutDown =>
          logger.info(s"❗ $command")
          //🔨if (command.clusterAction.nonEmpty && !clusterNode.isWorkingNode)
          //🔨  Task.pure(Left(PassiveClusterNodeShutdownNotAllowedProblem))
          //🔨else {
            //🔨if (command.dontNotifyActiveNode && clusterNode.isPassive) {
            //🔨  clusterNode.dontNotifyActiveNodeAboutShutdown()
            //🔨}
            clusterNode.stopRecovery(ProgramTermination(restart = command.restart)) >>
              executeCommand1(command, meta)
          //🔨}

        //🔨case AgentCommand.ClusterAppointNodes(idToUri, activeId) =>
        //🔨  Task(clusterNode.workingClusterNode)
        //🔨    .flatMapT(_.appointNodes(idToUri, activeId))
        //🔨    .rightAs(AgentCommand.Response.Accepted)

        case ClusterAppointNodes(idToUri, activeId) =>
          Task(clusterNode.workingClusterNode)
            .flatMapT(_.appointNodes(idToUri, activeId))
            .rightAs(AgentCommand.Response.Accepted)

        case ClusterSwitchOver =>
          Task(clusterNode.workingClusterNode)
            .flatMapT(_.switchOver)
            .rightAs(AgentCommand.Response.Accepted)

        case _ =>
          executeCommand1(command, meta)
      }).map(_.map((_: AgentCommand.Response).asInstanceOf[command.Response])))

  override def toString =
    "Agent"
}

object RunningAgent {
  private val logger = Logger(getClass)

  def resource(conf: AgentConfiguration, testWiring: TestWiring = TestWiring.empty)
    (implicit scheduler: Scheduler)
  : Resource[Task, RunningAgent] = Resource.suspend(Task {
    import conf.{config, httpsConfig, implicitAkkaAskTimeout, journalConf, journalMeta}

    // FIXME Use AgentPath as cluster node UserId --> wie AgentDedicated abwarten? Brauchen wir das?
    val clusterConf = {
      val userId = config.as[UserId]("js7.auth.cluster.user")
      ClusterConf.fromConfig(userId, config).orThrow
    }

    // Recover and initialize other stuff in parallel
    val clock = testWiring.alarmClock getOrElse AlarmClock(
      Some(config.getDuration("js7.time.clock-setting-check-interval").toFiniteDuration))(
      scheduler)

    val eventIdClock = testWiring.eventIdClock getOrElse EventIdClock(clock)
    val testEventBus = new StandardEventBus[Any]

    val recoveringResource =
      ClusterNode.recoveringResource[AgentState](
        akkaResource = Akkas.actorSystemResource(conf.name, config),
        (uri, label, actorSystem) => AgentClient.resource(
          uri, clusterConf.peersUserAndPassword, label, httpsConfig)(actorSystem),
        configDirectory = conf.configDirectory,
        journalMeta, journalConf, clusterConf, eventIdClock, testEventBus, config)

    def initialize(): Unit = {
      if (!StartUp.isMain) logger.debug("JS7 Agent starting ..." + "\n" + "┈" * 80)
      conf.createDirectories()
      conf.journalMeta.deleteJournalIfMarked().orThrow
    }

    for {
      _ <- Resource.eval(Task(initialize()))
      x <- recoveringResource
      (initiallyRecovered, actorSystem, clusterNode) = x
      iox <- IOExecutor.resource[Task](config, conf.name + "-I/O")
      blockingJobScheduler <- Resource
        .make(
          acquire = Task(newUnlimitedScheduler("JS7 blocking job")))(
          release = scheduler => Task(scheduler.shutdown()))
        .map(CorrelId.enableScheduler)

      agent <- resource2(clusterNode, initiallyRecovered, testWiring, conf, testEventBus, clock,
        blockingJobScheduler)(
        actorSystem, iox, scheduler)
    } yield agent
  })

  private def resource2(
    clusterNode: ClusterNode[AgentState],
    recoveredExtract: Recovered.Extract,
    testWiring: TestWiring,
    conf: AgentConfiguration,
    testEventBus: StandardEventBus[Any],
    clock: AlarmClock,
    blockingJobScheduler: Scheduler)
    (implicit actorSystem: ActorSystem, iox: IOExecutor, scheduler: Scheduler)
  : Resource[Task, RunningAgent] = {
    import conf.config

    def startMainActor(
      persistenceAllocated: Allocated[Task, FileStatePersistence[AgentState]])
    : MainActorStarted = {
      val mainActorReadyPromise = Promise[MainActor.Ready]()
      val terminationPromise = Promise[ProgramTermination]()
      val actor = actorSystem.actorOf(
        Props {
          new MainActor(recoveredExtract.totalRunningSince,
            persistenceAllocated, conf, testWiring.commandHandler,
            mainActorReadyPromise, terminationPromise,
            conf.toJobLauncherConf(iox, blockingJobScheduler, clock)
              .orThrow,
            testEventBus, clock)(scheduler, iox)
        },
        "main").taggedWith[MainActor]

      actor ! MainActor.Input.Start(persistenceAllocated.allocatedThing.unsafeCurrentState())

      MainActorStarted(
        actor,
        Task.fromFuture(mainActorReadyPromise.future),
        terminationPromise.future)
    }

    val mainActorStarted: Task[Either[ProgramTermination, MainActorStarted]] =
      logger.traceTaskWithResult(
        clusterNode.untilActivated
          .map(_.map(workingClusterNode =>
            startMainActor(workingClusterNode.persistenceAllocated)))
          .onErrorRecover { case t: RestartAfterJournalTruncationException =>
            logger.info(t.getMessage)
            Left(ProgramTermination(restart = true))
          }
      ).memoize

    @deprecated val whenReady = Promise[Unit] // NOT USED ?

    val untilReady: Task[MainActor.Ready] =
      mainActorStarted.flatMap {
        case Left(_: ProgramTermination) => Task.raiseError(new IllegalStateException(
          "Agent has been terminated"))
        case Right(mainActorStarted) => mainActorStarted.whenReady
      }

    // The AgentOrderKeeper if started
    val currentMainActor: Task[Checked[MainActorStarted]] =
      logger.traceTask(
        clusterNode.currentState
          .map(_.map(_.clusterState))
          .flatMapT { clusterState =>
            import clusterNode.clusterConf.{isBackup, ownId}
            if (!clusterState.isActive(ownId, isBackup = isBackup))
              Task.left(ClusterNodeIsNotActiveProblem)
            else
              mainActorStarted.map {
                case Left(_) => Left(ShuttingDownProblem)
                case Right(o) => Right(o)
              }
          }
          .tapError(t => Task {
            logger.debug(s"currentOrderKeeperActor => ${t.toStringWithCauses}", t)
            whenReady.tryFailure(t)
          }))

    val untilMainActorTerminated = logger.traceTask(
      mainActorStarted.flatMap {
        case Left(termination) => Task.pure(termination)
        case Right(o) =>
          Task
            .fromFuture(o.termination)
            .tapError(t => Task(
              logger.error(s"ControllerOrderKeeper failed with ${t.toStringWithCauses}", t)))
      }
        .tapError(t => Task(whenReady.tryFailure(t)))
    ).uncancelable /*a test may use this in `race`, unintentionally canceling this*/
      .memoize

    val gateKeeperConf = GateKeeper.Configuration.fromConfig(config, SimpleUser.apply)
    val sessionRegister = SessionRegister.start[AgentSession](actorSystem, AgentSession.apply, config)

    val agentOverview = Task(AgentOverview(
      startedAt = ServiceMain.startedAt,
      version = BuildInfo.prettyVersion,
      buildId = BuildInfo.buildId,
      //isTerminating = isTerminating,
      system = systemInformation(),
      java = javaInformation()))

    def executeCommand(cmd: AgentCommand, meta: CommandMeta)
    : Task[Checked[AgentCommand.Response]] =
      (cmd match {
        case cmd: AgentCommand.ShutDown =>
          logger.info(s"❗ $cmd")
          //🔨if (cmd.clusterAction.nonEmpty && !clusterNode.isWorkingNode)
          //🔨  Task.pure(Left(PassiveClusterNodeShutdownNotAllowedProblem))
          //🔨else {
          //🔨if (cmd.dontNotifyActiveNode && clusterNode.isPassive) {
          //🔨  clusterNode.dontNotifyActiveNodeAboutShutdown()
          //🔨}
          clusterNode.stopRecovery(ProgramTermination(restart = cmd.restart)) >>
            currentMainActor
              .flatMap(_.traverse(_.whenReady.map(_.api)))
              .flatMap {
                case Left(ClusterNodeIsNotActiveProblem | ShuttingDownProblem
                          | BackupClusterNodeNotAppointed) =>
                  Task.right(AgentCommand.Response.Accepted)

                case Left(problem @ ClusterNodeIsNotReadyProblem /*???*/) =>
                  logger.error(s"❓ $cmd => $problem")
                  Task.right(AgentCommand.Response.Accepted)

                case Left(problem) =>
                  Task.pure(Left(problem))

                case Right(api) =>
                  api(meta).commandExecute(cmd)
              }
        //🔨}

        case ClusterAppointNodes(idToUri, activeId) =>
          Task(clusterNode.workingClusterNode)
            .flatMapT(_.appointNodes(idToUri, activeId))
            .rightAs(AgentCommand.Response.Accepted)

        case ClusterSwitchOver =>
          Task.left(Problem("Agent still not support ClusterSwitchOver command"))
          // Notify AgentOrderKeeper ???
          //Task(clusterNode.workingClusterNode)
          //  .flatMapT(_.switchOver)
          //  .rightAs(AgentCommand.Response.Accepted)

        case _ =>
          currentMainActor
            .flatMap(_.traverse(_.whenReady.map(_.api)))
            .flatMapT(api => api(meta).commandExecute(cmd))
      }).map(_.map((_: AgentCommand.Response).asInstanceOf[cmd.Response]))

    for {
      sessionToken <- sessionRegister
        .placeSessionTokenInDirectory(SimpleUser.System, conf.workDirectory)

      webServer <- AgentWebServer
        .resource(
          agentOverview, conf, gateKeeperConf, executeCommand, clusterNode,
          sessionRegister,
          recoveredExtract.eventWatch)(actorSystem)
        .evalTap(webServer => Task {
          conf.workDirectory / "http-uri" :=
            webServer.localHttpUri.fold(_ => "", o => s"$o/agent")
        })
      agent <- Service.resource(Task(
        new RunningAgent(
          recoveredExtract.eventWatch,
          clusterNode,
          webServer, /*Task(mainActor),*/
          untilMainActorTerminated.runToFuture,
          untilReady, executeCommand, sessionRegister, sessionToken,
          testEventBus,
          actorSystem, config)))
    } yield agent
  }

  final case class TestWiring(
    alarmClock: Option[AlarmClock] = None,
    eventIdClock: Option[EventIdClock] = None,
    commandHandler: Option[CommandHandler] = None,
    authenticator: Option[AgentConfiguration => Authenticator[SimpleUser]] = None)
  object TestWiring {
    val empty = TestWiring()
  }

  private case class MainActorStarted(
    actor: ActorRef @@ MainActor,
    whenReady: Task[MainActor.Ready],
    termination: Future[ProgramTermination])
}
