package js7.tests

import java.nio.file.Files.{createDirectories, createDirectory}
import js7.agent.TestAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.crypt.silly.{SillySignature, SillySigner}
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Allocated
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
import js7.tests.testenv.TestController
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
final class ControllerAgentWithoutAuthenticationTest extends OurTestSuite
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

  private def runMyTest(isPublic: Boolean)(body: (TestController, Int) => Unit): Unit = {
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

      val controllerPort :: agentPort :: Nil = FreeTcpPortFinder.findFreeTcpPorts(2): @unchecked
      val agentConfiguration = AgentConfiguration.fromCommandLine(CommandLineArguments(
        "--config-directory=" + dir / "agent/config" ::
        "--data-directory=" + dir / "agent/data" ::
        "--http-port=" + agentPort :: Nil))
      val controllerConfiguration = ControllerConfiguration
        .fromCommandLine(
          CommandLineArguments(
            "--config-directory=" + dir / "controller/config" ::
              "--data-directory=" + dir / "controller/data" ::
              "--http-port=" + controllerPort :: Nil),
          config"""js7.auth.users.TEST-USER = "plain:TEST-PASSWORD" """)

      val subagentId = SubagentId("SUBAGENT")
      val agentRef = AgentRef(agentPath, directors = Seq(subagentId))
      val subagentItem = SubagentItem(subagentId, agentPath, Uri(s"http://127.0.0.1:$agentPort"))
      TestAgent.blockingRun(agentConfiguration, 99.s) { _ =>
        RunningController.blockingRun(controllerConfiguration, 99.s) { runningController =>
          val testController = new TestController(
            new Allocated(runningController, Task.unit),
            Admission(
              runningController.localUri,
              Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD"))))
          )
          testController.waitUntilReady()

          testController.updateItemsAsSystemUser(Observable(
            ItemOperation.AddOrChangeSimple(agentRef),
            ItemOperation.AddOrChangeSimple(subagentItem),
            ItemOperation.AddVersion(versionId),
            ItemOperation.AddOrChangeSigned(itemSigner.toSignedString(workflow)))
          ).await(99.s).orThrow

          body(testController, agentPort)
          testController.terminate().await(99.s)
        }
      }
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
  private val orderId = OrderId("🔷")
}
