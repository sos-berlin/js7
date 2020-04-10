package com.sos.jobscheduler.tests

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryDirectory
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder
import com.sos.jobscheduler.core.crypt.silly.{SillySignature, SillySigner}
import com.sos.jobscheduler.core.filebased.FileBasedSigner
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.master.MasterFileBaseds
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand.ReplaceRepo
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentCouplingFailed
import com.sos.jobscheduler.tests.MasterAgentWithoutAuthenticationTest._
import java.nio.file.Files.{createDirectories, createDirectory}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class MasterAgentWithoutAuthenticationTest extends FreeSpec
{
  "jobscheduler.webserver.auth.public = true" in {
    runMyTest(isPublic = true) { (master, _) =>
      master.addOrder(FreshOrder(orderId, workflow.path)).runSyncUnsafe(99.seconds).orThrow
      master.eventWatch.await[OrderFinished](_.key == orderId)
    }
  }

  "jobscheduler.webserver.auth.public = false" in {
    runMyTest(isPublic = false) { (master, agentPort) =>
      assert(master.eventWatch.await[AgentCouplingFailed]().head.value.event.problem
        == Problem(s"HTTP 401 Unauthorized: http://127.0.0.1:$agentPort/agent/api/command: The resource requires authentication, which was not supplied with the request"))
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
        dir / "agent/config/agent.conf" := "jobscheduler.webserver.auth.public = true\n"
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
            "jobscheduler.configuration.trusted-signature-keys.Silly = " +
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
