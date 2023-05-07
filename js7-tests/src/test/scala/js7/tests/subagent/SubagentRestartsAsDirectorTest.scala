package js7.tests.subagent

import java.nio.file.Files.createDirectories
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{CoupleController, DedicateAgentDirector}
import js7.base.Js7Version
import js7.base.auth.SessionToken
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.problem.Problem
import js7.base.session.SessionCommand
import js7.base.session.SessionCommand.Login
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.web.{HttpClient, Uri}
import js7.common.akkautils.ProvideActorSystem
import js7.common.configuration.Js7Configuration
import js7.common.http.AkkaHttpClient
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.subagent.BareSubagent
import js7.subagent.ConvertibleToDirector.ConvertToDirector
import js7.tests.testenv.DirectoryProvider
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.Future

final class SubagentRestartsAsDirectorTest extends OurTestSuite
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
        val controllerId = ControllerId("CONTROLLER")
        val agentPath = AgentPath("AGENT")
        val privat = createDirectories(directory / "data" / "private")
        val port = findFreeTcpPort()
        val uri = Uri(s"http://localhost:$port")
        val conf = directoryProvider
          .toSubagentConf(
            agentPath, directory, trustedSignatureDir = privat, port = port,
            this.config, name = "SubagentRestartsAsDirectorTest")
          .finishAndProvideFiles

        val commands = Seq(
          CoupleController(agentPath, AgentRunId.empty, 0L),
          DedicateAgentDirector(Nil, controllerId, agentPath))

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
                  Login(None, Some(Js7Version)))
                .onErrorRestartLoop(99) {
                  case (t, n, retry) if n > 0
                    && (t.getMessage.contains("Connection refused")
                     || t.getMessage.contains("WebServiceStillNotAvailable")) =>
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
              "Subagent is converting to an Agent Director - try again in a second")))
          }
          val result = subagentTerminated.await(99.s)
          assert(result == Left(ConvertToDirector))
        }
      }
    }
  }
}
