package js7.agent.tests

import js7.agent.client.AgentClient
import js7.agent.configuration.Akkas.newAgentActorSystem
import js7.agent.data.commands.AgentCommand.{AttachOrder, AttachSignedItem, RegisterAsController, ShutDown}
import js7.agent.tests.TerminateTest._
import js7.base.auth.{SimpleUser, UserId}
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.problem.Checked.Ops
import js7.base.thread.Futures.implicits._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.common.akkautils.Akkas
import js7.common.system.ServerOperatingSystem.operatingSystem
import js7.core.command.CommandMeta
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.event.EventRequest
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.value.StringValue
import js7.data.workflow.test.TestSetting._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class TerminateTest extends AnyFreeSpec with AgentTester
{
  override def beforeAll() = {
    (agentDirectory / "config" / "private" / "private.conf") ++= """
        |js7.auth.users.TEST-USER = "plain:TEST-PASSWORD"
        |""".stripMargin
    APathExecutable.toFile(agentDirectory / "config" / "executables").writeExecutable(AScript)
    BPathExecutable.toFile(agentDirectory / "config" / "executables").writeExecutable(AScript)
    super.beforeAll()
  }

  "ShutDown" in {
    implicit val actorSystem = newAgentActorSystem("TerminateTest")
    val userId = UserId("TEST-USER")
    closer onClose Akkas.terminateAndWait(actorSystem, 10.s)

    val client = AgentClient(agentUri = agent.localUri, Some(userId -> SecretString("TEST-PASSWORD")))
    client.login() await 99.s
    client.commandExecute(RegisterAsController(agentPath)) await 99.s

    val eventWatch = agent.api(CommandMeta(SimpleUser(userId)))
      .eventWatchForController(ControllerId.fromUserId(userId))
      .await(99.s).orThrow

    client.commandExecute(AttachSignedItem(itemSigner.sign(SimpleTestWorkflow)))
      .await(99.s).orThrow

    val orderIds = for (i <- 0 until 3) yield OrderId(s"TEST-ORDER-$i")
    (for (orderId <- orderIds) yield
      client.commandExecute(AttachOrder(
        Order(
          orderId,
          SimpleTestWorkflow.id,
          Order.Ready,
          Map("a" -> StringValue("A"))),
        TestAgentPath))
    ) await 99.s

    val whenStepEnded: Future[Seq[OrderProcessed]] =
      Future.sequence(
        for (orderId <- orderIds) yield
          eventWatch.whenKeyedEvent[OrderProcessed](EventRequest.singleClass(timeout = Some(90.s)), orderId)
            .runToFuture: Future[OrderProcessed])
    sleep(2.s)
    assert(!whenStepEnded.isCompleted)

    client.commandExecute(ShutDown(Some(SIGKILL))).await(99.s).orThrow
    val stepEnded = whenStepEnded await 99.s
    assert(stepEnded.forall(_.outcome.isInstanceOf[Outcome.Killed]))
    agent.terminated await 99.s
    client.close()
  }
}

object TerminateTest
{
  private val agentPath = AgentPath("AGENT")
  private val AScript = operatingSystem.sleepingShellScript(10.s)
}
