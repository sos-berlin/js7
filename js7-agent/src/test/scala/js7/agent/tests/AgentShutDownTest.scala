package js7.agent.tests

import js7.agent.RunningAgent
import js7.agent.client.AgentClient
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.Pekkos.newAgentActorSystem
import js7.agent.data.commands.AgentCommand.{AttachItem, AttachOrder, AttachSignedItem, DedicateAgentDirector, ShutDown}
import js7.agent.tests.AgentShutDownTest.*
import js7.base.auth.UserId
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.problem.Checked.Ops
import js7.base.system.ServerOperatingSystem.operatingSystem
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.common.pekkoutils.Pekkos
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.controller.ControllerId
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.value.StringValue
import js7.data.workflow.position.Position
import js7.data.workflow.test.TestSetting.*
import monix.execution.Scheduler.Implicits.traced
import org.apache.pekko.actor.ActorSystem
import org.scalatest.BeforeAndAfterAll

final class AgentShutDownTest extends OurTestSuite with BeforeAndAfterAll with TestAgentDirectoryProvider
{
  override def beforeAll() = {
    (agentDirectory / "config" / "private" / "private.conf") ++= """
        |js7.auth.users.TEST-USER = "plain:TEST-PASSWORD"
        |js7.web.server.delay-shutdown = 1000ms
        |""".stripMargin
    APathExecutable.toFile(agentDirectory / "config" / "executables").writeUtf8Executable(AScript)
    BPathExecutable.toFile(agentDirectory / "config" / "executables").writeUtf8Executable(AScript)
    super.beforeAll()
  }

  "ShutDown" in {
    val agentConfiguration = AgentConfiguration.forTest(agentDirectory, "AgentShutDownTest")
    var agent = RunningAgent.startForTest(agentConfiguration).await(10.s)

    implicit val actorSystem: ActorSystem = newAgentActorSystem("AgentShutDownTest")
    val userId = UserId("TEST-USER")
    closer onClose Pekkos.terminateAndWait(actorSystem, 10.s)

    val client = AgentClient(agentUri = agent.localUri, Some(userId -> SecretString("TEST-PASSWORD")))
    client.login() await 99.s
    client
      .commandExecute(DedicateAgentDirector(Some(SubagentId("SUBAGENT")), controllerId, agentPath))
      .await(99.s)

    client
      .commandExecute(AttachItem(AgentRef(agentPath, Seq(SubagentId("SUBAGENT")))))
      .await(99.s)

    val subagentId = SubagentId("SUBAGENT")
    client.commandExecute(AttachItem(SubagentItem(subagentId, agentPath, agent.localUri)))
      .await(99.s).orThrow
    client.commandExecute(AttachSignedItem(itemSigner.sign(SimpleTestWorkflow)))
      .await(99.s).orThrow

    val orderIds = for (i <- 0 until 3) yield OrderId(s"TEST-ORDER-$i")
    (for (orderId <- orderIds) yield
      client.commandExecute(AttachOrder(
        Order(
          orderId,
          SimpleTestWorkflow.id /: Position(0),
          Order.Ready,
          Map("a" -> StringValue("A"))),
        TestAgentPath))
    ) await 99.s

    sleep(2.s)

    client.commandExecute(ShutDown(Some(SIGKILL))).await(99.s).orThrow
    agent.terminated await 99.s

    agent = RunningAgent.startForTest(agentConfiguration).await(10.s)
    for (orderId <- orderIds) {
      val processed = agent.eventWatch.await[OrderProcessed](_.key == orderId)
      assert(processed.head.value.event.outcome.isInstanceOf[Outcome.Killed])
    }

    agent.terminate().await(99.s)
    client.close()
  }
}

object AgentShutDownTest
{
  private val agentPath = AgentPath("AGENT")
  private val controllerId = ControllerId("CONTROLLER")
  private val AScript = operatingSystem.sleepingShellScript(10.s)
}
