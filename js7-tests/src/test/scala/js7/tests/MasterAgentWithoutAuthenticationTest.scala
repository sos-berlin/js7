package js7.tests

import java.nio.file.Files.{createDirectories, createDirectory}
import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.crypt.silly.{SillySignature, SillySigner}
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.withTemporaryDirectory
import js7.common.scalautil.Futures.implicits._
import js7.common.utils.FreeTcpPortFinder
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.filebased.{FileBasedSigner, VersionId}
import js7.data.job.ExecutablePath
import js7.data.master.MasterFileBaseds
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.master.RunningMaster
import js7.master.configuration.MasterConfiguration
import js7.master.data.MasterCommand.ReplaceRepo
import js7.master.data.events.MasterAgentEvent.AgentCouplingFailed
import js7.tests.MasterAgentWithoutAuthenticationTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class MasterAgentWithoutAuthenticationTest extends AnyFreeSpec
{
  "js7.webserver.auth.public = true" in {
    runMyTest(isPublic = true) { (master, _) =>
      master.addOrder(FreshOrder(orderId, workflow.path)).runSyncUnsafe(99.seconds).orThrow
      master.eventWatch.await[OrderFinished](_.key == orderId)
    }
  }

  "js7.webserver.auth.public = false" in {
    runMyTest(isPublic = false) { (master, agentPort) =>
      assert(master.eventWatch.await[AgentCouplingFailed]().head.value.event.problem
        == Problem(s"HTTP 401 Unauthorized: #2 POST http://127.0.0.1:$agentPort/agent/api/command => The resource requires authentication, which was not supplied with the request"))
    }
  }

  private def runMyTest(isPublic: Boolean)(body: (RunningMaster, Int) => Unit): Unit = {
    withTemporaryDirectory("MasterAgentWithoutAuthenticationTest-") { dir =>
      createDirectories(dir / "master/config/private")
      createDirectories(dir / "master/data/state")
      createDirectories(dir / "agent/config/private")
      createDirectories(dir / "agent/config/executables")
      createDirectories(dir / "agent/data/state")

      if (isPublic) {
        dir / "agent/config/agent.conf" := "js7.webserver.auth.public = true\n"
      }
      (dir / "agent/config/executables/EXECUTABLE.cmd").writeExecutable(":")

      val fileBasedSigner = {
        val signature = SillySignature("✘✘✘")
        for (x <- Array("master", "agent")) {
          val keyDirectory = dir / x / "config/private/silly-signatures"
          createDirectory(keyDirectory)
          val keyFile =keyDirectory / "silly-signature.txt"
          keyFile := signature.string
          dir / x / "config/private/private.conf" ++=
            "js7.configuration.trusted-signature-keys.Silly = " +
              "\"" + keyDirectory.toString.replace("""\""", """\\""") + "\"\n"
        }
        new FileBasedSigner(new SillySigner(signature), MasterFileBaseds.jsonCodec)
      }

      val masterPort :: agentPort :: Nil = FreeTcpPortFinder.findFreeTcpPorts(2)
      val agentConfiguration = AgentConfiguration.fromCommandLine(CommandLineArguments(
        "-config-directory=" + dir / "agent/config" ::
        "-data-directory=" + dir / "agent/data" ::
        "-http-port=" + agentPort :: Nil))
      val masterConfiguration = MasterConfiguration.fromCommandLine(CommandLineArguments(
        "-config-directory=" + dir / "master/config" ::
        "-data-directory=" + dir / "master/data" ::
        "-http-port=" + masterPort :: Nil))

      val agentRef = AgentRef(agentRefPath ~ versionId, Uri(s"http://127.0.0.1:$agentPort"))
      val agent = RunningAgent(agentConfiguration) await 99.seconds
      val master = RunningMaster(masterConfiguration) await 99.seconds
      master.waitUntilReady()

      val replaceRepo = ReplaceRepo(versionId, (agentRef :: workflow :: Nil) map fileBasedSigner.sign)
      master.executeCommandAsSystemUser(replaceRepo).runSyncUnsafe(99.seconds).orThrow

      body(master, agentPort)

      master.terminate().runSyncUnsafe(99.seconds)
      agent.terminate().runSyncUnsafe(99.seconds)
    }
  }
}

object MasterAgentWithoutAuthenticationTest
{
  private val versionId = VersionId("INITIAL")
  private val agentRefPath = AgentRefPath("/AGENT")
  private val executablePath = ExecutablePath("/EXECUTABLE.cmd")
  private val workflow = Workflow.of(WorkflowPath("/WORKFLOW") ~ versionId,
    Execute(WorkflowJob(agentRefPath, executablePath)))
  private val orderId = OrderId("🔵")
}
