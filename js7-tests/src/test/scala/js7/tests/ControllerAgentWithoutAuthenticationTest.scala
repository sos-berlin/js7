package js7.tests

import java.nio.file.Files.{createDirectories, createDirectory}
import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.crypt.silly.{SillySignature, SillySigner}
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.withTemporaryDirectory
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder
import js7.controller.RunningController
import js7.controller.configuration.ControllerConfiguration
import js7.controller.data.ControllerCommand.{ReplaceRepo, UpdateSimpleItems}
import js7.controller.data.ControllerState.versionedItemJsonCodec
import js7.controller.data.events.AgentRefStateEvent.AgentCouplingFailed
import js7.data.agent.{AgentId, AgentRef}
import js7.data.item.{VersionId, VersionedItemSigner}
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.ControllerAgentWithoutAuthenticationTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class ControllerAgentWithoutAuthenticationTest extends AnyFreeSpec
{
  "js7.web.server.auth.public = true" in {
    runMyTest(isPublic = true) { (controller, _) =>
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
      controller.eventWatch.await[OrderFinished](_.key == orderId)
    }
  }

  "js7.web.server.auth.public = false" in {
    runMyTest(isPublic = false) { (controller, agentPort) =>
      assert(controller.eventWatch.await[AgentCouplingFailed]().head.value.event.problem
        == Problem(s"HTTP 401 Unauthorized: POST http://127.0.0.1:$agentPort/agent/api/command => The resource requires authentication, which was not supplied with the request"))
    }
  }

  private def runMyTest(isPublic: Boolean)(body: (RunningController, Int) => Unit): Unit = {
    withTemporaryDirectory("ControllerAgentWithoutAuthenticationTest-") { dir =>
      createDirectories(dir / "controller/config/private")
      createDirectories(dir / "controller/data/state")
      createDirectories(dir / "agent/config/private")
      createDirectories(dir / "agent/config/executables")
      createDirectories(dir / "agent/data/state")

      if (isPublic) {
        dir / "agent/config/agent.conf" := "js7.web.server.auth.public = true\n"
      }
      (dir / "agent/config/executables/EXECUTABLE.cmd").writeExecutable(":")

      val itemSigner = {
        val signature = SillySignature("✘✘✘")
        for (x <- Array("controller", "agent")) {
          val keyDirectory = dir / x / "config/private/silly-signatures"
          createDirectory(keyDirectory)
          val keyFile =keyDirectory / "silly-signature.txt"
          keyFile := signature.string
          dir / x / "config/private/private.conf" ++=
            "js7.configuration.trusted-signature-keys.Silly = " +
              "\"" + keyDirectory.toString.replace("""\""", """\\""") + "\"\n"
        }
        new VersionedItemSigner(new SillySigner(signature), versionedItemJsonCodec)
      }

      val controllerPort :: agentPort :: Nil = FreeTcpPortFinder.findFreeTcpPorts(2)
      val agentConfiguration = AgentConfiguration.fromCommandLine(CommandLineArguments(
        "--config-directory=" + dir / "agent/config" ::
        "--data-directory=" + dir / "agent/data" ::
        "--http-port=" + agentPort :: Nil))
      val controllerConfiguration = ControllerConfiguration.fromCommandLine(CommandLineArguments(
        "--config-directory=" + dir / "controller/config" ::
        "--data-directory=" + dir / "controller/data" ::
        "--http-port=" + controllerPort :: Nil))

      val agentRef = AgentRef(agentId, Uri(s"http://127.0.0.1:$agentPort"))
      val agent = RunningAgent(agentConfiguration) await 99.seconds
      val controller = RunningController(controllerConfiguration) await 99.seconds
      controller.waitUntilReady()

      controller.executeCommandAsSystemUser(UpdateSimpleItems(Seq(agentRef))).await(99.s).orThrow
      val replaceRepo = ReplaceRepo(versionId, Seq(workflow) map itemSigner.sign)
      controller.executeCommandAsSystemUser(replaceRepo).await(99.s).orThrow

      body(controller, agentPort)

      controller.terminate() await 99.s
      agent.terminate() await 99.s
    }
  }
}

object ControllerAgentWithoutAuthenticationTest
{
  private val versionId = VersionId("INITIAL")
  private val agentId = AgentId("AGENT")
  private val executablePath = ExecutablePath("EXECUTABLE.cmd")
  private val workflow = Workflow.of(WorkflowPath("/WORKFLOW") ~ versionId,
    Execute(WorkflowJob(agentId, executablePath)))
  private val orderId = OrderId("🔵")
}
