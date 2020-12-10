package js7.agent.tests

import com.google.inject.{AbstractModule, Injector, Provides}
import javax.inject.Singleton
import js7.agent.client.AgentClient
import js7.agent.configuration.Akkas.newAgentActorSystem
import js7.agent.data.commands.AgentCommand.{AttachOrder, RegisterAsController, ShutDown}
import js7.agent.tests.TerminateTest._
import js7.base.auth.UserId
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.process.ProcessSignal.SIGKILL
import js7.base.time.ScalaTime._
import js7.common.akkautils.Akkas
import js7.common.event.collector.EventCollector
import js7.common.guice.GuiceImplicits.RichInjector
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.system.ServerOperatingSystem.operatingSystem
import js7.core.event.ActorEventCollector
import js7.data.agent.AgentId
import js7.data.event.EventRequest
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.value.StringValue
import js7.data.workflow.test.TestSetting._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class TerminateTest extends AnyFreeSpec with AgentTester
{
  override def beforeAll() = {
    (agentDirectory / "config" / "private" / "private.conf") ++= """
        |js7.auth.users.TEST-USER = "plain:TEST-PASSWORD"
        |""".stripMargin
    AExecutablePath.toFile(agentDirectory / "config" / "executables").writeExecutable(AScript)
    BExecutablePath.toFile(agentDirectory / "config" / "executables").writeExecutable(AScript)
    super.beforeAll()
  }

  "ShutDown" in {
    implicit val actorSystem = newAgentActorSystem("TerminateTest")
    closer onClose Akkas.terminateAndWait(actorSystem, 10.s)

    val client = AgentClient(agentUri = agent.localUri, Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD")))
    client.login() await 99.s
    client.commandExecute(RegisterAsController(agentId)) await 99.s

    val eventCollector = newEventCollector(agent.injector)

    val orderIds = for (i <- 0 until 3) yield OrderId(s"TEST-ORDER-$i")
    (for (orderId <- orderIds) yield
      client.commandExecute(AttachOrder(
        Order(
          orderId,
          SimpleTestWorkflow.id,
          Order.Ready,
          Map("a" -> StringValue("A"))),
        TestAgentId,
        itemSigner.sign(SimpleTestWorkflow)))
    ) await 99.s

    val whenStepEnded: Future[Seq[OrderProcessed]] =
      Future.sequence(
        for (orderId <- orderIds) yield
          eventCollector.whenKeyedEvent[OrderProcessed](EventRequest.singleClass(timeout = Some(90.s)), orderId)
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
  private val agentId = AgentId("AGENT")
  private val AScript = operatingSystem.sleepingShellScript(10.seconds)

  private def newEventCollector(injector: Injector): EventCollector =
    injector.createChildInjector(new AbstractModule {
      override def configure() = bind(classOf[EventCollector.Configuration]) toInstance
        new EventCollector.Configuration(queueSize = 100000)

      @Provides @Singleton
      def eventCollector(factory: ActorEventCollector.Factory): EventCollector =
        factory.apply()
    }).instance[EventCollector]
}
