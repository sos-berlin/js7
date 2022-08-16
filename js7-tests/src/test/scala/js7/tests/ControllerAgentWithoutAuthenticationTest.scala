package js7.tests

import java.nio.file.Files.{createDirectories, createDirectory}
import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.crypt.silly.{SillySignature, SillySigner}
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.Test
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.utils.FreeTcpPortFinder
import js7.controller.RunningController
import js7.controller.configuration.ControllerConfiguration
import js7.data.agent.AgentRefStateEvent.AgentCouplingFailed
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.controller.ControllerState.versionedItemJsonCodec
import js7.data.item.{ItemOperation, ItemSigner, VersionId}
import js7.data.job.PathExecutable
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.ControllerAgentWithoutAuthenticationTest.*
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
final class ControllerAgentWithoutAuthenticationTest extends Test
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
      createDirectories(dir / "controller/data/work")
      createDirectories(dir / "agent/config/private")
      createDirectories(dir / "agent/config/executables")
      createDirectories(dir / "agent/data/state")
      createDirectories(dir / "agent/data/work")

      if (isPublic) {
        dir / "agent/config/agent.conf" := "js7.web.server.auth.public = true\n"
      }
      (dir / "agent/config/executables/EXECUTABLE.cmd").writeUtf8Executable(":")

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
        new ItemSigner(new SillySigner(signature), versionedItemJsonCodec)
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

      val subagentId = SubagentId("SUBAGENT")
      val agentRef = AgentRef(agentPath, directors = Seq(subagentId))
      val subagentItem = SubagentItem(subagentId, agentPath, Uri(s"http://127.0.0.1:$agentPort"))
      val agent = RunningAgent(agentConfiguration) await 99.s
      val controller = RunningController.start(controllerConfiguration) await 99.s
      controller.waitUntilReady()

      controller.updateItemsAsSystemUser(Observable(
        ItemOperation.AddOrChangeSimple(agentRef),
        ItemOperation.AddOrChangeSimple(subagentItem),
        ItemOperation.AddVersion(versionId),
        ItemOperation.AddOrChangeSigned(itemSigner.toSignedString(workflow)))
      ).await(99.s).orThrow

      body(controller, agentPort)

      controller.terminate() await 99.s
      agent.terminate() await 99.s
    }
  }
}

object ControllerAgentWithoutAuthenticationTest
{
  private val versionId = VersionId("INITIAL")
  private val agentPath = AgentPath("AGENT")
  private val pathExecutable = PathExecutable("EXECUTABLE.cmd")
  private val workflow = Workflow.of(WorkflowPath("WORKFLOW") ~ versionId,
    Execute(WorkflowJob(agentPath, pathExecutable)))
  private val orderId = OrderId("🔵")
}
