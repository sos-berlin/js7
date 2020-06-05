package js7.agent

import akka.actor.{ActorRef, ActorSystem, Props}
import com.google.inject.Stage.PRODUCTION
import com.google.inject.{Guice, Injector, Module}
import js7.agent.RunningAgent._
import js7.agent.configuration.inject.AgentModule
import js7.agent.configuration.{AgentConfiguration, AgentStartInformation}
import js7.agent.data.AgentTermination
import js7.agent.data.commands.AgentCommand
import js7.agent.web.AgentWebServer
import js7.base.auth.{SessionToken, SimpleUser, UserId}
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer
import js7.base.utils.ScalaUtils.RichThrowable
import js7.base.web.Uri
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.guice.GuiceImplicits._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Futures.promiseFuture
import js7.common.scalautil.Logger
import js7.core.command.CommandMeta
import js7.core.startup.StartUp
import com.typesafe.config.Config
import java.nio.file.Files.deleteIfExists
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise, blocking}

/**
 * JS7 Agent Server.
 *
 * Integration test in engine-tests, for example js7.tests.jira.js1291.JS1291AgentIT.
 *
 * @author Joacim Zschimmer
 */
final class RunningAgent private(
  val webServer: AgentWebServer,
  mainActor: ActorRef,
  terminated1: Future[AgentTermination.Terminate],
  val api: CommandMeta => DirectAgentApi,
  sessionRegister: SessionRegister[SimpleSession],
  val sessionToken: SessionToken,
  closer: Closer,
  @TestOnly val injector: Injector)
extends AutoCloseable {

  implicit val scheduler = injector.instance[Scheduler]
  val config: Config = injector.instance[Config]
  lazy val localUri: Uri = webServer.localUri
  @TestOnly
  lazy val actorSystem = injector.instance[ActorSystem]

  val terminated: Future[AgentTermination.Terminate] =
    for (o <- terminated1) yield {
      close()
      o
    }
  //val sessionTokenHeader: HttpHeader = RawHeader(SessionToken.HeaderName, sessionToken.secret.string)

  logger.debug("Ready")

  def close() = closer.close()

  def terminate(sigkillProcessesAfter: Option[FiniteDuration] = Some(5.seconds)): Task[AgentTermination.Terminate] =
    if (terminated.isCompleted)  // Works only if previous termination has been completed
      Task.fromFuture(terminated)
    else {
      logger.debug("terminate")
      for {
        _ <- directExecuteCommand(AgentCommand.ShutDown(sigtermProcesses = true, sigkillProcessesAfter = sigkillProcessesAfter))
              .map(_.orThrow)
        t <- Task.fromFuture(terminated)
      } yield t
    }

  /** Circumvents the CommandHandler which is possibly replaced by a test via DI. */  // TODO Do we need all this code?
  private def directExecuteCommand(command: AgentCommand): Task[Checked[AgentCommand.Response]] =
    Task.deferFuture(
      promiseFuture[Checked[AgentCommand.Response]](promise =>
        mainActor ! MainActor.Input.ExternalCommand(UserId.Anonymous, command, promise)))

  def executeCommandAsSystemUser(command: AgentCommand): Task[Checked[AgentCommand.Response]] =
    for {
      checkedSession <- sessionRegister.systemSession
      checkedChecked <- checkedSession.map(session => executeCommand(command, CommandMeta(session.currentUser))).evert
    } yield checkedChecked.flatten

  def executeCommand(command: AgentCommand, meta: CommandMeta): Task[Checked[AgentCommand.Response]] =
    api(meta).commandExecute(command)
}

object RunningAgent {
  private val logger = Logger(getClass)

  def run[A](configuration: AgentConfiguration, timeout: Option[FiniteDuration] = None)(body: RunningAgent => A): A =
    autoClosing(apply(configuration) await timeout) { agent =>
      val a = body(agent)
      agent.terminated await 99.s
      a
    }

  def startForTest(conf: AgentConfiguration)(implicit ec: ExecutionContext): Future[RunningAgent] =
    startForTest(new AgentModule(conf))

  def startForTest(module: Module)(implicit ec: ExecutionContext): Future[RunningAgent] = {
    val whenAgent = apply(module)
    for (agent <- whenAgent; t <- agent.terminated.failed) logger.error(t.toStringWithCauses, t)
    whenAgent
  }

  def apply(configuration: AgentConfiguration): Future[RunningAgent] =
    apply(new AgentModule(configuration))

  def apply(module: Module): Future[RunningAgent] = {
    AgentStartInformation.initialize()
    val injector = Guice.createInjector(PRODUCTION, module)
    val agentConfiguration = injector.instance[AgentConfiguration]

    if (agentConfiguration.scriptInjectionAllowed) logger.info("SIGNED SCRIPT INJECTION IS ALLOWED")
    StartUp.logStartUp(agentConfiguration.configDirectory, Some(agentConfiguration.dataDirectory))

    val actorSystem = injector.instance[ActorSystem]
    val closer = injector.instance[Closer]
    val webServer = injector.instance[AgentWebServer]

    val sessionRegister = injector.instance[SessionRegister[SimpleSession]]
    val mainActorReadyPromise = Promise[MainActor.Ready]()
    val terminationPromise = Promise[AgentTermination.Terminate]()
    val mainActor = actorSystem.actorOf(
      Props { new MainActor(agentConfiguration, sessionRegister, injector, mainActorReadyPromise, terminationPromise) },
      "main")
    implicit val scheduler = injector.instance[Scheduler]

    agentConfiguration.stateDirectory / "http-uri" := webServer.localHttpUri.fold(_ => "", o => s"$o/agent")

    val sessionTokenFile = agentConfiguration.stateDirectory / "session-token"
    val sessionToken = blocking {
      sessionRegister.createSystemSession(SimpleUser.System, sessionTokenFile)
        .runToFuture await agentConfiguration.akkaAskTimeout.duration
    }
    closer onClose { deleteIfExists(sessionTokenFile) }

    for {
      ready <- mainActorReadyPromise.future
      api = ready.api
      _ <- webServer.start(api)
    } yield
      new RunningAgent(webServer, mainActor, terminationPromise.future, api, sessionRegister, sessionToken, closer, injector)
  }
}
