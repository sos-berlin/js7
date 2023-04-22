package js7.agent

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.server.directives.SecurityDirectives.Authenticator
import cats.effect.Resource
import cats.effect.concurrent.Deferred
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
import js7.base.auth.{SessionToken, SimpleUser}
import js7.base.eventbus.StandardEventBus
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
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
import js7.cluster.ClusterNode
import js7.cluster.ClusterNode.RestartAfterJournalTruncationException
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.SessionRegister
import js7.common.akkautils.Akkas
import js7.common.system.JavaInformations.javaInformation
import js7.common.system.SystemInformations.systemInformation
import js7.common.system.ThreadPools.unlimitedSchedulerResource
import js7.common.system.startup.{ServiceMain, StartUp}
import js7.core.command.CommandMeta
import js7.data.Problems.{BackupClusterNodeNotAppointed, ClusterNodeIsNotActiveProblem, ClusterNodeIsNotReadyProblem, PassiveClusterNodeShutdownNotAllowedProblem}
import js7.data.agent.AgentClusterConf
import js7.data.node.NodeId
import js7.data.subagent.SubagentId
import js7.journal.EventIdClock
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.recover.Recovered
import js7.journal.state.FileStateJournal
import js7.journal.watch.EventWatch
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.Atomic
import scala.concurrent.{Future, Promise}

final class RunningAgent private(
  val eventWatch: EventWatch,
  clusterNode: ClusterNode[AgentState],
  val journal: Task[FileStateJournal[AgentState]],
  webServer: AkkaWebServer & AkkaWebServer.HasUri,
  actorTermination: Task[ProgramTermination],
  val untilReady: Task[MainActor.Ready],
  val executeCommand: (AgentCommand, CommandMeta) => Task[Checked[AgentCommand.Response]],
  sessionRegister: SessionRegister[AgentSession],
  val sessionToken: SessionToken,
  val testEventBus: StandardEventBus[Any],
  val actorSystem: ActorSystem,
  val config: Config)
  (implicit val scheduler: Scheduler)
extends MainService with Service.StoppableByRequest
{
  lazy val localUri: Uri = webServer.localUri

  val untilTerminated: Task[ProgramTermination] =
    actorTermination

  def agentState: Task[Checked[AgentState]] =
    clusterNode.currentState

  private val isTerminating = Atomic(false)
  @volatile private var isActorTerminated = false

  logger.debug("Ready")

  protected def start: Task[Service.Started] =
    Task.defer {
      for (_ <- actorTermination.attempt) isActorTerminated = true
      startService(
        untilStopRequested
          .*>(shutdown)
          /*// Stop WebServer early to close TCP connections — required for testing
          .*>(webServer.stop)*/)
    }

  private def shutdown: Task[Unit] =
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
    clusterAction: Option[ShutDown.ClusterAction] = None,
    suppressSnapshot: Boolean = false)
  : Task[ProgramTermination] =
    logger.debugTask(
      terminating {
        executeCommand(
          AgentCommand.ShutDown(processSignal, clusterAction, suppressSnapshot = suppressSnapshot),
          CommandMeta(SimpleUser.System)
        ).map(_.orThrow)
      })

  private def terminating(body: Task[Unit]): Task[ProgramTermination] =
    Task.defer {
      Task
        .unless(isTerminating.getAndSet(true) || isActorTerminated)(
          body)
        .*>(untilTerminated)
        .logWhenItTakesLonger
    }

  private[agent] def executeCommandAsSystemUser(command: AgentCommand)
  : Task[Checked[AgentCommand.Response]] =
    for {
      checkedSession <- sessionRegister.systemSession
      checkedChecked <- checkedSession.traverse(session =>
        executeCommand(command, CommandMeta(session.currentUser)))
    } yield checkedChecked.flatten

  override def toString =
    "Agent"
}

object RunningAgent {
  private val logger = Logger(getClass)

  def resource(conf: AgentConfiguration, testWiring: TestWiring = TestWiring.empty)
    (implicit scheduler: Scheduler)
  : Resource[Task, RunningAgent] = Resource.suspend(Task {
    import conf.{clusterConf, config, httpsConfig, implicitAkkaAskTimeout, journalConf, journalMeta}

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
      (recoveredExtract, actorSystem, clusterNode) = x
      iox <- IOExecutor.resource[Task](config, conf.name + "-I/O")
      blockingJobScheduler <- unlimitedSchedulerResource[Task]("JS7 blocking job", config)
      agent <- resource2(clusterNode, recoveredExtract, testWiring, conf, testEventBus, clock,
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
      failedNodeId: Option[NodeId],
      journalAllocated: Allocated[Task, FileStateJournal[AgentState]])
    : MainActorStarted = {
      val failedOverSubagentId: Option[SubagentId] = {
        val directors = journalAllocated.allocatedThing.unsafeCurrentState().meta.directors
        failedNodeId.flatMap {
          case AgentClusterConf.primaryNodeId => directors.get(0)
          case AgentClusterConf.backupNodeId => directors.get(1)
          case nodeId => throw new AssertionError(s"🔥 Unexpected $nodeId")
        }
      }

      val mainActorReadyPromise = Promise[MainActor.Ready]()
      val terminationPromise = Promise[ProgramTermination]()
      val actor = actorSystem.actorOf(
        Props {
          new MainActor(
            recoveredExtract.totalRunningSince,
            failedOverSubagentId,
            journalAllocated, conf, testWiring.commandHandler,
            mainActorReadyPromise, terminationPromise,
            clusterNode,
            conf.toJobLauncherConf(iox, blockingJobScheduler, clock)
              .orThrow,
            testEventBus, clock)(scheduler, iox)
        },
        "main").taggedWith[MainActor]

      actor ! MainActor.Input.Start(journalAllocated.allocatedThing.unsafeCurrentState())

      MainActorStarted(
        actor,
        Task.fromFuture(mainActorReadyPromise.future),
        terminationPromise.future)
    }

    val journalDeferred = Deferred.unsafe[Task, FileStateJournal[AgentState]]

    val mainActorStarted: Task[Either[ProgramTermination, MainActorStarted]] =
      logger.traceTaskWithResult(
        clusterNode.untilActivated
          .flatMapT(workingClusterNode =>
            journalDeferred.complete(workingClusterNode.journalAllocated.allocatedThing)
              .*>(Task(
                startMainActor(
                  workingClusterNode.failedNodeId,
                  workingClusterNode.journalAllocated)))
              .map(Right(_)))
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
      logger.debugTask("executeCommand", cmd.getClass.shortClassName)(cmd
        .match_ {
          case cmd: AgentCommand.ShutDown =>
            logger.info(s"❗ $cmd")
            if (cmd.clusterAction.nonEmpty && !clusterNode.isWorkingNode)
              Task.left(PassiveClusterNodeShutdownNotAllowedProblem)
            else {
              //⚒️if (cmd.dontNotifyActiveNode && clusterNode.isPassive) {
              //⚒️  clusterNode.dontNotifyActiveNodeAboutShutdown()
              //⚒️}
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
            }

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
        }
        .map(_.map((_: AgentCommand.Response).asInstanceOf[cmd.Response])))

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
          journalDeferred.get,
          webServer, /*Task(mainActor),*/
          untilMainActorTerminated,
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
