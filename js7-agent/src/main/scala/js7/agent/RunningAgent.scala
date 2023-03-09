package js7.agent

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.server.directives.SecurityDirectives.Authenticator
import cats.effect.Resource
import cats.syntax.traverse.*
import com.softwaremill.diffx.generic.auto.*
import com.typesafe.config.Config
import js7.agent.RunningAgent.*
import js7.agent.command.CommandHandler
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.agent.data.views.AgentOverview
import js7.agent.main.AgentMain
import js7.agent.web.AgentWebServer
import js7.agent.web.common.AgentSession
import js7.base.BuildInfo
import js7.base.auth.{SessionToken, SimpleUser, UserId}
import js7.base.eventbus.StandardEventBus
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.{CorrelId, Logger}
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.service.{MainService, Service}
import js7.base.thread.Futures.promiseFuture
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ProgramTermination}
import js7.base.web.Uri
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.SessionRegister
import js7.common.akkautils.{Akkas, DeadLetterActor}
import js7.common.system.JavaInformations.javaInformation
import js7.common.system.SystemInformations.systemInformation
import js7.common.system.ThreadPools.newUnlimitedScheduler
import js7.common.system.startup.StartUp
import js7.core.command.CommandMeta
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.recover.{Recovered, StateRecoverer}
import js7.journal.state.{FileStatePersistence, ReadableStatePersistence}
import js7.journal.{EventIdClock, EventIdGenerator}
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.Atomic
import scala.concurrent.{Future, Promise}

/**
 * JS7 Agent.
 *
 * Integration test in engine-tests, for example js7.tests.jira.js1291.JS1291AgentIT.
 *
 * @author Joacim Zschimmer
 */
final class RunningAgent private(
  persistence: ReadableStatePersistence[AgentState],
  webServer: AkkaWebServer & AkkaWebServer.HasUri,
  mainActor: ActorRef,
  val terminated: Future[ProgramTermination],
  val api: CommandMeta => DirectAgentApi,
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

  val eventWatch = persistence.eventWatch
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

  private[agent] def currentAgentState(): AgentState =
    persistence.unsafeCurrentState()

  private[agent] def terminate(
    processSignal: Option[ProcessSignal] = None,
    suppressSnapshot: Boolean = false)
  : Task[ProgramTermination] =
    terminating {
      logger.debug("terminate")
      directExecuteCommand(AgentCommand.ShutDown(
          processSignal,
          suppressSnapshot = suppressSnapshot)
        ).map(_.orThrow)
    }

  private def terminating(body: Task[Unit]): Task[ProgramTermination] =
    Task.defer {
      Task
        .unless(isTerminating.getAndSet(true) || terminated.isCompleted)(
          body)
        .*>(untilTerminated)
    }

  /** Circumvents the CommandHandler which is possibly replaced by a test via DI. */  // TODO Do we need all this code?
  private def directExecuteCommand(command: AgentCommand): Task[Checked[AgentCommand.Response]] =
    Task.deferFuture(
      promiseFuture[Checked[AgentCommand.Response]] { promise =>
        mainActor !
          MainActor.Input.ExternalCommand(command, UserId.Anonymous, CorrelId.current, promise)
      })

  private[agent] def executeCommandAsSystemUser(command: AgentCommand): Task[Checked[AgentCommand.Response]] =
    for {
      checkedSession <- sessionRegister.systemSession
      checkedChecked <- checkedSession.traverse(session => executeCommand(command, CommandMeta(session.currentUser)))
    } yield checkedChecked.flatten

  private[agent] def executeCommand(command: AgentCommand, meta: CommandMeta): Task[Checked[AgentCommand.Response]] =
    api(meta).commandExecute(command)

  override def toString =
    "Agent"
}

object RunningAgent {
  private val logger = Logger(getClass)

  def resource(conf: AgentConfiguration, testWiring: TestWiring = TestWiring.empty)
    (implicit scheduler: Scheduler)
  : Resource[Task, RunningAgent] = {
    import conf.config
    for {
      _ <- Resource.eval(Task(conf.createDirectories()))
      actorSystem <- Akkas.actorSystemResource(conf.name, config)
        .evalTap(actorSystem => Task(
          DeadLetterActor.subscribe(actorSystem)))
      iox <- IOExecutor.resource[Task](config, conf.name + "-I/O")
      blockingJobScheduler <- Resource
        .make(
          acquire = Task(newUnlimitedScheduler("JS7 blocking job")))(
          release = scheduler => Task(scheduler.shutdown()))
        .map(CorrelId.enableScheduler)
      agent <- resource2(
        SessionRegister.start[AgentSession](actorSystem, AgentSession.apply, config),
        conf,
        GateKeeper.Configuration.fromConfig(config, SimpleUser.apply),
        blockingJobScheduler,
        new StandardEventBus[Any],
        actorSystem,
        testWiring)(
        scheduler, iox)
    } yield agent
  }

  private def resource2(
    sessionRegister: SessionRegister[AgentSession],
    conf: AgentConfiguration,
    gateKeeperConf: GateKeeper.Configuration[SimpleUser],
    blockingJobScheduler: Scheduler,
    testEventBus: StandardEventBus[Any],
    actorSystem: ActorSystem,
    testWiring: TestWiring)
    (implicit scheduler: Scheduler, iox: IOExecutor)
  : Resource[Task, RunningAgent] = {
    import conf.{config, implicitAkkaAskTimeout, journalConf, journalMeta}

    val clock = testWiring.alarmClock getOrElse AlarmClock(
      Some(config.getDuration("js7.time.clock-setting-check-interval").toFiniteDuration))(
      scheduler)

    val eventIdGenerator = new EventIdGenerator(
      testWiring.eventIdClock getOrElse EventIdClock(clock))

    Resource.suspend(Task {
      if (!StartUp.isMain) {
        logger.debug("JS7 Agent starting ..." +
          "\n" + "┈" * 80)
      }

      // Run under scheduler from start (and let debugger show Controller's thread names)
      conf.journalMeta.deleteJournalIfMarked().orThrow

      implicit val implcitAkka = actorSystem

      def mainActorResource(
        recovered: Recovered[AgentState],
        persistenceAllocated: Allocated[Task, FileStatePersistence[AgentState]])
      : Resource[Task, (ActorRef, Task[MainActor.Ready], Future[ProgramTermination])] =
        Resource
          .make(
            acquire = Task {
              val mainActorReadyPromise = Promise[MainActor.Ready]()
              val terminationPromise = Promise[ProgramTermination]()
              val actor = actorSystem.actorOf(
                Props {
                  new MainActor(persistenceAllocated, conf, testWiring.commandHandler,
                    mainActorReadyPromise, terminationPromise,
                    conf.toJobLauncherConf(iox, blockingJobScheduler, clock)
                      .orThrow,
                    testEventBus, clock)
                },
                "main")
              (actor,
                Task.fromFuture(mainActorReadyPromise.future),
                terminationPromise.future)
            })(
            release = { case (actor, _, _)  => Task(
              actorSystem.stop(actor))
            })
          .evalTap { case (actor, _, _) => Task(
            actor ! MainActor.Input.Start(recovered))
          }

      val agentOverview = Task(AgentOverview(
        startedAt = AgentMain.startedAt,
        version = BuildInfo.prettyVersion,
        buildId = BuildInfo.buildId,
        //isTerminating = isTerminating /*FIXME*/ ,
        system = systemInformation(),
        java = javaInformation()))

      for {
        recovered <- StateRecoverer.resource[AgentState](journalMeta, config)
        persistenceAllocated <- Resource.make(
          acquire = FileStatePersistence
            .resource(recovered, journalConf, eventIdGenerator, new StandardEventBus)
            .toAllocated)(
          release = _.stop)
        persistence = persistenceAllocated.allocatedThing
        x <- mainActorResource(recovered, persistenceAllocated)
        (mainActor, untilActorReady, whenActorTerminated) = x
        api <- Resource.eval(untilActorReady.map(_.api))
        webServer <- AgentWebServer
          .resource(agentOverview, conf, gateKeeperConf, api, sessionRegister, persistence.eventWatch)
          .evalTap(webServer => Task {
            conf.workDirectory / "http-uri" :=
              webServer.localHttpUri.fold(_ => "", o => s"$o/agent")
          })
        sessionToken <- sessionRegister
          .placeSessionTokenInDirectory(SimpleUser.System, conf.workDirectory)
        agent <- Service.resource(Task(
          new RunningAgent(persistence, webServer, mainActor,
            whenActorTerminated, api, sessionRegister, sessionToken,
            testEventBus,
            actorSystem, config)))
      } yield agent
    })
  }

  final case class TestWiring(
    alarmClock: Option[AlarmClock] = None,
    eventIdClock: Option[EventIdClock] = None,
    commandHandler: Option[CommandHandler] = None,
    authenticator: Option[AgentConfiguration => Authenticator[SimpleUser]] = None)
  object TestWiring {
    val empty = TestWiring()
  }
}
