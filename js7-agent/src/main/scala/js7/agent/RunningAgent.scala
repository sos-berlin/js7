package js7.agent

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.server.directives.SecurityDirectives.Authenticator
import cats.effect.Resource
import cats.effect.concurrent.Deferred
import cats.syntax.traverse.*
import com.softwaremill.diffx.generic.auto.*
import com.softwaremill.tagging.{@@, Tagger}
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
import js7.base.eventbus.StandardEventBus
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixBase.syntax.{RichMonixResource, RichMonixTask}
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.problem.Problems.ShuttingDownProblem
import js7.base.service.{MainService, Service}
import js7.base.time.AlarmClock
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ProgramTermination}
import js7.base.web.Uri
import js7.cluster.ClusterNode
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.system.JavaInformations.javaInformation
import js7.common.system.SystemInformations.systemInformation
import js7.common.system.startup.StartUp
import js7.core.command.CommandMeta
import js7.core.license.LicenseChecker
import js7.data.Problems.{BackupClusterNodeNotAppointed, ClusterNodeIsNotActiveProblem, ClusterNodeIsNotReadyProblem, PassiveClusterNodeShutdownNotAllowedProblem}
import js7.data.agent.AgentClusterConf
import js7.data.node.NodeId
import js7.data.subagent.SubagentId
import js7.journal.EventIdClock
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.state.FileJournal
import js7.journal.watch.JournalEventWatch
import js7.license.LicenseCheckContext
import js7.subagent.Subagent
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.Atomic
import scala.concurrent.{Future, Promise}

final class RunningAgent private(
  clusterNode: ClusterNode[AgentState],
  val journal: Task[FileJournal[AgentState]],
  getLocalUri: () => Uri,
  untilMainActorTerminated: Task[ProgramTermination],
  val untilReady: Task[MainActor.Ready],
  val executeCommand: (AgentCommand, CommandMeta) => Task[Checked[AgentCommand.Response]],
  forDirector: Subagent.ForDirector,
  val testEventBus: StandardEventBus[Any],
  val actorSystem: ActorSystem,
  val conf: AgentConfiguration)
  (implicit val scheduler: Scheduler)
extends MainService with Service.StoppableByRequest
{
  lazy val localUri: Uri = getLocalUri()
  val eventWatch: JournalEventWatch = clusterNode.recoveredExtract.eventWatch
  private val isTerminating = Atomic(false)

  val untilTerminated: Task[ProgramTermination] =
    untilMainActorTerminated
      .guarantee(Task(isTerminating := true))
      .memoize

  def agentState: Task[Checked[AgentState]] =
    clusterNode.currentState

  protected def start: Task[Service.Started] =
    startService(
      forDirector.subagent.untilTerminated
        .*>(stop/*TODO Subagent should terminate this Director*/)
        .startAndForget
        .*>(Task.race(
          untilStopRequested *> shutdown,
          untilTerminated))
        .void)

  private def shutdown: Task[Unit] =
    logger.debugTask(
      terminating(
        executeCommandAsSystemUser(ShutDown(Some(SIGTERM)))
          .attempt
          .map {
            case Left(throwable) => logger.warn(throwable.toStringWithCauses)
            case Right(Left(problem)) => logger.warn(problem.toString)
            case Right(Right(_)) =>
          }
          .startAndForget
      ).void)

  private[agent] def terminate(
    processSignal: Option[ProcessSignal] = None,
    clusterAction: Option[ShutDown.ClusterAction] = None,
    suppressSnapshot: Boolean = false)
  : Task[ProgramTermination] =
    logger.debugTask(
      terminating {
        executeCommand(
          ShutDown(processSignal, clusterAction, suppressSnapshot = suppressSnapshot),
          CommandMeta(SimpleUser.System)
        ).map(_.orThrow)
      })

  private def terminating(body: Task[Unit]): Task[ProgramTermination] =
    Task.defer {
      Task
        .unless(isTerminating.getAndSet(true))(
          body)
        .*>(untilTerminated)
        .logWhenItTakesLonger
    }

  private[agent] def executeCommandAsSystemUser(command: AgentCommand)
  : Task[Checked[AgentCommand.Response]] =
    for {
      checkedSession <- forDirector.sessionRegister.systemSession
      checkedChecked <- checkedSession.traverse(session =>
        executeCommand(command, CommandMeta(session.currentUser)))
    } yield checkedChecked.flatten

  def systemSessionToken: SessionToken =
    forDirector.systemSessionToken

  override def toString =
    "Agent"
}

object RunningAgent {
  private val logger = Logger(getClass)

  def resource(
    conf: AgentConfiguration,
    testWiring: TestWiring = TestWiring.empty)
    (implicit scheduler: Scheduler)
  : Resource[Task, RunningAgent] = Resource.suspend(Task {
    import conf.{clusterConf, config, httpsConfig, implicitAkkaAskTimeout, journalLocation}

    // Recover and initialize other stuff in parallel
    val clock = testWiring.alarmClock getOrElse AlarmClock(
      Some(config.getDuration("js7.time.clock-setting-check-interval").toFiniteDuration))(
      scheduler)

    val eventIdClock = testWiring.eventIdClock getOrElse EventIdClock(clock)
    val testEventBus = new StandardEventBus[Any]

    def initialize(): Unit = {
      if (!StartUp.isMain) logger.debug("JS7 Agent starting ...\n" + "┈" * 80)
      conf.createDirectories()
      conf.journalLocation.deleteJournalIfMarked().orThrow
    }

    for {
      _ <- Resource.eval(Task(initialize()))
      subagent <- Subagent.resource(conf.subagentConf, testEventBus)
      forDirector = subagent.forDirector/*TODO Subagent itself should start Director when requested*/
      clusterNode <- ClusterNode.recoveringResource[AgentState](
        akkaResource = Resource.eval(Task.pure(forDirector.actorSystem)),
        (uri, label, actorSystem) => AgentClient.resource(
          uri, clusterConf.peersUserAndPassword, label, httpsConfig)(actorSystem),
        new LicenseChecker(LicenseCheckContext(conf.configDirectory)),
        journalLocation, clusterConf, eventIdClock, testEventBus)
      agent <- resource2(forDirector, clusterNode, testWiring, conf, testEventBus, clock)(
        scheduler)
    } yield agent
  }).executeOn(scheduler)

  private def resource2(
    forDirector: Subagent.ForDirector,
    clusterNode: ClusterNode[AgentState],
    testWiring: TestWiring,
    conf: AgentConfiguration,
    testEventBus: StandardEventBus[Any],
    clock: AlarmClock)
    (implicit scheduler: Scheduler)
  : Resource[Task, RunningAgent] = {
    import clusterNode.actorSystem
    import conf.config

    def startMainActor(
      failedNodeId: Option[NodeId],
      journalAllocated: Allocated[Task, FileJournal[AgentState]])
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
            forDirector,
            failedOverSubagentId,
            clusterNode,
            journalAllocated, conf, testWiring.commandHandler,
            mainActorReadyPromise, terminationPromise,
             clock)(
            scheduler)
        },
        "main").taggedWith[MainActor]

      actor ! MainActor.Input.Start(journalAllocated.allocatedThing.unsafeCurrentState())

      MainActorStarted(
        actor,
        Task.fromFuture(mainActorReadyPromise.future),
        terminationPromise.future)
    }

    val journalDeferred = Deferred.unsafe[Task, FileJournal[AgentState]]

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
          //.onErrorRecover { case t: RestartAfterJournalTruncationException =>
          //  logger.info(t.getMessage)
          //  Left(t.termination)
          //}
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

    val untilMainActorTerminated =
      logger
        .traceTask(
          mainActorStarted
            .flatMap {
              case Left(termination) => Task.pure(termination)
              case Right(o) =>
                Task
                  .fromFuture(o.termination)
                  .tapError(t => Task(
                    logger.error(s"MainActor failed with ${t.toStringWithCauses}", t)))
            }
            .tapError(t => Task(whenReady.tryFailure(t))))
        .uncancelable /*a test may use this in `race`, unintentionally canceling this*/
        .memoize

    val gateKeeperConf = GateKeeper.Configuration.fromConfig(config, SimpleUser.apply)

    val agentOverview = Task(AgentOverview(
      startedAt = StartUp.startedAt,
      version = BuildInfo.prettyVersion,
      buildId = BuildInfo.buildId,
      //isTerminating = isTerminating,
      system = systemInformation(),
      java = javaInformation()))

    def executeCommand(cmd: AgentCommand, meta: CommandMeta)
    : Task[Checked[AgentCommand.Response]] =
      logger.debugTask("executeCommand", cmd.getClass.shortClassName)(cmd
        .match_ {
          case cmd: ShutDown =>
            if (cmd.clusterAction.nonEmpty && !clusterNode.isWorkingNode)
              Task.left(PassiveClusterNodeShutdownNotAllowedProblem)
            else {
              logger.info(s"❗ $cmd")
              //⚒️if (cmd.dontNotifyActiveNode && clusterNode.isPassive) {
              //⚒️  clusterNode.dontNotifyActiveNodeAboutShutdown()
              //⚒️}
              clusterNode.stopRecovery(ProgramTermination(restart = cmd.restart)) >>
                currentMainActor
                  .flatMap(_.traverse(_.whenReady.map(_.api)))
                  .flatMap {
                    case Left(problem @ (ClusterNodeIsNotActiveProblem | ShuttingDownProblem
                              | BackupClusterNodeNotAppointed)) =>
                      logger.debug(s"❓$problem")
                      Task.right(AgentCommand.Response.Accepted)

                    case Left(problem @ ClusterNodeIsNotReadyProblem /*???*/) =>
                      logger.error(s"❓ $cmd => $problem")
                      Task.right(AgentCommand.Response.Accepted)

                    case Left(problem) =>
                      Task.left(problem)

                    case Right(api) =>
                      api(meta).commandExecute(cmd)
                  }
            }

          case _ =>
            currentMainActor
              .flatMap(_.traverse(_.whenReady.map(_.api)))
              .flatMapT(api => api(meta).commandExecute(cmd))
        }
        .map(_.map((_: AgentCommand.Response).asInstanceOf[cmd.Response]))
        .logWhenItTakesLonger(s"${cmd.getClass.simpleScalaName} command"))

    for {
      agent <- Service.resource(Task(
        new RunningAgent(
          clusterNode,
          journalDeferred.get,
          () => forDirector.subagent.localUri,
          untilMainActorTerminated,
          untilReady, executeCommand,
          forDirector,
          testEventBus,
          actorSystem, conf)))
      _ <- forDirector.subagent.directorRegisteringResource(
        (binding, whenShuttingDown) => Task.pure(
          new AgentRoute(
            agentOverview,
            binding,
            whenShuttingDown,
            agent.executeCommand,
            clusterNode,
            conf,
            gateKeeperConf,
            forDirector.sessionRegister
          ).agentRoute))
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
