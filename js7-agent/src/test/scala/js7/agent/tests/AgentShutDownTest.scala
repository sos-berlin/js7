package js7.agent.tests

import js7.agent.TestAgent
import js7.agent.client.AgentClient
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand.{AttachItem, AttachOrder, AttachSignedItem, DedicateAgentDirector, ShutDown}
import js7.agent.tests.AgentShutDownTest.*
import js7.base.auth.{Admission, UserId}
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsBlocking.BlockingTaskResource
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.actorSystemResource
import js7.base.system.ServerOperatingSystem.operatingSystem
import js7.base.test.OurTestSuite
import js7.base.system.ServerOperatingSystem.operatingSystem
import js7.data.agent.AgentPath
import js7.data.controller.{ControllerId, ControllerRunId}
import js7.data.event.JournalId
import js7.data.order.OrderEvent.OrderProcessingStarted
import js7.data.order.{Order, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.value.StringValue
import js7.data.workflow.position.Position
import js7.data.workflow.test.TestSetting.*
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.BeforeAndAfterAll

final class AgentShutDownTest extends OurTestSuite, BeforeAndAfterAll, TestAgentDirectoryProvider:

  override def beforeAll() =
    (agentDirectory / "config" / "private" / "private.conf") ++= """
        |js7.auth.users.TEST-USER = "plain:TEST-PASSWORD"
        |js7.web.server.delay-shutdown = 1000ms
        |""".stripMargin
    APathExecutable.toFile(agentDirectory / "config" / "executables").writeUtf8Executable(AScript)
    BPathExecutable.toFile(agentDirectory / "config" / "executables").writeUtf8Executable(AScript)
    super.beforeAll()

  "ShutDown" in:
    val agentConfiguration = AgentConfiguration.forTest(agentDirectory, "AgentShutDownTest")
    val orderIds = for i <- 0 until 3 yield OrderId(s"TEST-ORDER-$i")

    TestAgent.blockingRun(agentConfiguration,  99.s) { agent =>
      actorSystemResource("AgentShutDownTest").blockingUse(99.s) { implicit actorSystem =>
        val userId = UserId("TEST-USER")
        closer onClose Pekkos.terminateAndWait(actorSystem, 10.s)

        val client = AgentClient(Admission(
          agent.localUri,
          Some(userId -> SecretString("TEST-PASSWORD"))))
        client.login() await 99.s

        val controllerRunId = ControllerRunId(JournalId.random())
        client
          .repeatUntilAvailable(99.s)(
            client.commandExecute(DedicateAgentDirector(
              Seq(SubagentId("SUBAGENT")), controllerId, controllerRunId, agentPath)))
          .await(99.s)

        val subagentId = SubagentId("SUBAGENT")
        client
          .repeatUntilAvailable(99.s)(
            client.commandExecute(AttachItem(SubagentItem(subagentId, agentPath, agent.localUri))))
          .await(99.s).orThrow
        client.commandExecute(AttachSignedItem(itemSigner.sign(SimpleTestWorkflow)))
          .await(99.s).orThrow

        (for orderId <- orderIds yield
          client.commandExecute(AttachOrder(
            Order(
              orderId,
              SimpleTestWorkflow.id /: Position(0),
              Order.Ready,
              Map("a" -> StringValue("A"))),
            TestAgentPath))
        ) await 99.s

        for orderId <- orderIds do
          agent.eventWatch.await[OrderProcessingStarted](_.key == orderId)
        sleep(2.s)

        client.commandExecute(ShutDown(Some(SIGKILL))).await(99.s).orThrow
        agent.untilTerminated.await(99.s)
        client.close()
      }
    }

    //Times out, but why ??? Since v2.6.0-SNAPSHOT 2023-03-13
    //TestAgent.blockingRun(agentConfiguration, 99.s) { agent =>
    //  for (orderId <- orderIds) {
    //    val processed = agent.eventWatch.await[OrderProcessed](_.key == orderId)
    //    assert(processed.head.value.event.outcome.isInstanceOf[Outcome.Killed])
    //  }
    //}


object AgentShutDownTest:
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
  private val controllerId = ControllerId("CONTROLLER")
  private val AScript = operatingSystem.sleepingShellScript(10.s)
