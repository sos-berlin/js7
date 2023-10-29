package js7.agent

import akka.actor.{ActorRef, ActorSystem, Props}
import cats.syntax.traverse.*
import com.google.inject.Stage.PRODUCTION
import com.google.inject.util.Modules
import com.google.inject.util.Modules.EMPTY_MODULE
import com.google.inject.{Guice, Injector, Module}
import com.softwaremill.diffx.generic.auto.*
import com.typesafe.config.Config
import js7.agent.RunningAgent.*
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.inject.AgentModule
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.web.AgentWebServer
import js7.agent.web.common.AgentSession
import js7.base.auth.{SessionToken, SimpleUser, UserId}
import js7.base.eventbus.StandardEventBus
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.process.ProcessSignal
import js7.base.log.{CorrelId, Logger}
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.system.startup.StartUp
import js7.base.thread.Futures.implicits.*
import js7.base.thread.Futures.promiseFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Closer, ProgramTermination}
import js7.base.web.Uri
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.SessionRegister
import js7.common.guice.GuiceImplicits.*
import js7.core.cluster.watch.ClusterWatchRegister
import js7.core.command.CommandMeta
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.recover.StateRecoverer
import js7.journal.state.{FileStatePersistence, ReadableStatePersistence}
import js7.journal.{EventIdGenerator, StampedKeyedEventBus}
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

/**
 * JS7 Agent.
 *
 * Integration test in engine-tests, for example js7.tests.jira.js1291.JS1291AgentIT.
 *
 * @author Joacim Zschimmer
 */
final class RunningAgent private(
  val persistence: ReadableStatePersistence[AgentState],
  val webServer: AkkaWebServer & AkkaWebServer.HasUri,
  mainActor: ActorRef,
  terminated1: Future[ProgramTermination],
  val api: CommandMeta => DirectAgentApi,
  sessionRegister: SessionRegister[AgentSession],
  val sessionToken: SessionToken,
  closer: Closer,
  val injector: Injector)
extends AutoCloseable
{
  implicit val scheduler: Scheduler = injector.instance[Scheduler]
  val config: Config = injector.instance[Config]
  lazy val localUri: Uri = webServer.localUri
  @TestOnly
  lazy val actorSystem = injector.instance[ActorSystem]

  @TestOnly
  lazy val testEventBus = injector.instance[StandardEventBus[Any]]

  val terminated: Future[ProgramTermination] =
    for (o <- terminated1) yield {
      close()
      o
    }
  //val sessionTokenHeader: HttpHeader = RawHeader(SessionToken.HeaderName, sessionToken.secret.string)

  val eventWatch = persistence.eventWatch

  logger.debug("Ready")

  def currentAgentState(): AgentState =
    persistence.currentState

  def close() = closer.close()

  def terminate(processSignal: Option[ProcessSignal] = None): Task[ProgramTermination] =
    if (terminated.isCompleted)  // Works only if previous termination has been run
      Task.fromFuture(terminated)
    else {
      logger.debug("terminate")
      for {
        _ <- directExecuteCommand(AgentCommand.ShutDown(processSignal))
              .map(_.orThrow)
        t <- Task.fromFuture(terminated)
      } yield t
    }

  /** Circumvents the CommandHandler which is possibly replaced by a test via DI. */  // TODO Do we need all this code?
  private def directExecuteCommand(command: AgentCommand): Task[Checked[AgentCommand.Response]] =
    Task.deferFuture(
      promiseFuture[Checked[AgentCommand.Response]] { promise =>
        mainActor !
          MainActor.Input.ExternalCommand(command, UserId.Anonymous, CorrelId.current, promise)
      })

  def executeCommandAsSystemUser(command: AgentCommand): Task[Checked[AgentCommand.Response]] =
    for {
      checkedSession <- sessionRegister.systemSession
      checkedChecked <- checkedSession.traverse(session => executeCommand(command, CommandMeta(session.currentUser)))
    } yield checkedChecked.flatten

  def executeCommand(command: AgentCommand, meta: CommandMeta): Task[Checked[AgentCommand.Response]] =
    api(meta).commandExecute(command)
}

object RunningAgent {
  private val logger = Logger(getClass)

  def run[A](configuration: AgentConfiguration, timeout: Option[FiniteDuration] = None)(body: RunningAgent => A)(implicit s: Scheduler): A =
    autoClosing(apply(configuration) await timeout) { agent =>
      val tried = Try { body(agent) }
      for (t <- tried.failed) logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
      tried match {
        case Success(result) =>
          Try(agent.terminated await 3.s)
          agent.terminate() await 99.s
          result
        case Failure(throwable) =>
          // Avoid Akka 2.6 StackTraceError which occurs when agent.terminate() has not been executed:
          for (_ <- Try(agent.terminated await 3.s).failed) {
            agent.terminated.value match {
              case Some(terminated) =>
                for (t <- terminated.failed) logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
              case None =>
                try agent.terminate() await 99.s
                catch { case t: Throwable => throwable.addSuppressed(t) }
            }
          }
          throw throwable
      }
    }

  def startForTest(conf: AgentConfiguration,
    scheduler: Option[Scheduler] = None,
    module: Module = EMPTY_MODULE)(
    implicit ec: ExecutionContext)
  : Future[RunningAgent] =
    startForTest(
      Modules.`override`(new AgentModule(conf, scheduler))
        .`with`(module))

  def startForTest(module: Module)(implicit ec: ExecutionContext): Future[RunningAgent] = {
    val whenAgent = apply(module)
    for (agent <- whenAgent; t <- agent.terminated.failed) logger.error(t.toStringWithCauses, t)
    whenAgent
  }

  def apply(configuration: AgentConfiguration): Future[RunningAgent] =
    apply(new AgentModule(configuration))

  def apply(module: Module): Future[RunningAgent] = {
    if (!StartUp.isMain) {
      logger.debug("JS7 Agent starting ..." +
        "\n" + "┈" * 80)
    }
    val injector = Guice.createInjector(PRODUCTION, module)
    implicit val scheduler: Scheduler = injector.instance[Scheduler]

    // Run under scheduler from start (and let debugger show Controller's thread names)
    Future {
      val agentConfiguration = injector.instance[AgentConfiguration]
      import agentConfiguration.{config, implicitAkkaAskTimeout, journalConf, journalMeta}
      agentConfiguration.journalMeta.deleteJournalIfMarked()
        .orThrow
      val whenRecovered = Future(StateRecoverer.recover[AgentState](journalMeta, config))

      implicit val actorSystem: ActorSystem = injector.instance[ActorSystem]
      val closer = injector.instance[Closer]

      val recovered = whenRecovered.awaitInfinite
      recovered.eventWatch.closeWithCloser(closer)

      val persistence = FileStatePersistence
        .start(recovered, journalConf,
          injector.instance[EventIdGenerator],
          injector.instance[StampedKeyedEventBus])
        .awaitInfinite
        .closeWithCloser(closer)

      val gateKeeperConf = injector.instance[GateKeeper.Configuration[SimpleUser]]

      val mainActorReadyPromise = Promise[MainActor.Ready]()
      val terminationPromise = Promise[ProgramTermination]()
      val mainActor = actorSystem.actorOf(
        Props {
          new MainActor(persistence, agentConfiguration, injector, mainActorReadyPromise,
            terminationPromise)
        },
        "main")

      mainActor ! MainActor.Input.Start(recovered)

      val sessionRegister = injector.instance[SessionRegister[AgentSession]]

      val task = for {
        ready <- Task.fromFuture(mainActorReadyPromise.future)
        api = ready.api
        webServer = AgentWebServer(
          agentConfiguration,
          gateKeeperConf,
          api,
          injector.instance[SessionRegister[AgentSession]],
          injector.instance[ClusterWatchRegister],
          persistence.eventWatch
        ).closeWithCloser(closer)
        _ <- webServer.start
        sessionToken <- sessionRegister.placeSessionTokenInDirectoryLegacy(
          SimpleUser.System,
          agentConfiguration.workDirectory,
          closer)
      } yield {
        agentConfiguration.workDirectory / "http-uri" :=
          webServer.localHttpUri.fold(_ => "", o => s"$o/agent")

        new RunningAgent(persistence, webServer, mainActor,
          terminationPromise.future, api, sessionRegister, sessionToken,
          closer, injector)
      }
      task.runToFuture
    }.flatten
  }
}
