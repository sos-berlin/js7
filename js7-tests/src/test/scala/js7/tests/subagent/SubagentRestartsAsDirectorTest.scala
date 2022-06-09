package js7.tests.subagent

import java.nio.file.Files.createDirectories
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{CoupleController, DedicateAgentDirector}
import js7.base.BuildInfo
import js7.base.auth.SessionToken
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.problem.Problem
import js7.base.session.SessionCommand
import js7.base.session.SessionCommand.Login
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.version.Version
import js7.base.web.{HttpClient, Uri}
import js7.common.akkautils.ProvideActorSystem
import js7.common.configuration.Js7Configuration
import js7.common.http.AkkaHttpClient
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.subagent.BareSubagent
import js7.tests.testenv.DirectoryProvider
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future

final class SubagentRestartsAsDirectorTest extends AnyFreeSpec
with BeforeAndAfterAll
with ProvideActorSystem
{
  protected val config =config"""
    js7.web.server.auth.loopback-is-public = true
    """.withFallback(Js7Configuration.defaultConfig)

  override def afterAll(): Unit = {
    close()
    super.afterAll()
  }

  "Restart as Agent Director" in {
    val directoryProvider = new DirectoryProvider(agentPaths = Nil)
    autoClosing(directoryProvider) { _ =>
      withTemporaryDirectory("SubagentRestartsAsDirectorTest-") { directory =>
        val privat = createDirectories(directory / "data" / "private")
        val port = findFreeTcpPort()
        val uri = Uri(s"http://localhost:$port")
        val conf = directoryProvider.toSubagentConf(directory, trustedSignatureDir = privat, port = port, this.config, name = "SubagentRestartsAsDirectorTest")
          .finishAndProvideFiles

        val agentPath = AgentPath("AGENT")
        val controllerId = ControllerId("CONTROLLER")
        val commands = Seq(
          CoupleController(agentPath, AgentRunId.empty, 0L),
          DedicateAgentDirector(None, controllerId, agentPath))

        for (command <- commands) {
          val subagentTerminated = Future {
            BareSubagent.blockingRun(conf)
          }

          val client = new AkkaHttpClient.Standard(uri, actorSystem = actorSystem)
          implicit val sessionToken: Task[Option[SessionToken]] = Task.none

          locally {
            HttpClient
              .liftProblem(client
                .post[SessionCommand, SessionCommand.Response](
                  uri / "agent/api/session",
                  Login(None, Some(Version(BuildInfo.version))))
                .onErrorRestartLoop(99) {
                  case (t, n, retry) if n > 0 && t.getMessage.contains("Connection refused") =>
                    assert(subagentTerminated.value == None)
                    retry(n -1).delayExecution(500.ms)
                  case (t, _, _) => Task.raiseError(t)
                })
              .await(99.s)
              .orThrow
          }

          locally {
            val checked = HttpClient
              .liftProblem(client
                .post[AgentCommand, AgentCommand.Response](
                  uri / "agent/api/command",
                  command))
              .await(99.s)
            assert(checked == Left(Problem(
              "Subagent becomes a fresh Agent Director - try again after a second")))
          }
          val result = subagentTerminated.await(99.s)
          assert(result == Left(BareSubagent.StartAsAgentDirector))
        }
      }
    }
  }
}