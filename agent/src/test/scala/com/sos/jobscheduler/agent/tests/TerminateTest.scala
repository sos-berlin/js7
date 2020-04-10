package com.sos.jobscheduler.agent.tests

import com.google.inject.{AbstractModule, Injector, Provides}
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.configuration.Akkas.newAgentActorSystem
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{AttachOrder, RegisterAsMaster, ShutDown}
import com.sos.jobscheduler.agent.tests.TerminateTest._
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.common.system.OperatingSystem.operatingSystem
import com.sos.jobscheduler.core.event.ActorEventCollector
import com.sos.jobscheduler.data.event.EventRequest
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.test.TestSetting._
import javax.inject.Singleton
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class TerminateTest extends FreeSpec with AgentTester {

  override def beforeAll() = {
    (agentDirectory / "config" / "private" / "private.conf") ++= """
        |jobscheduler.auth.users.TEST-USER = "plain:TEST-PASSWORD"
        |""".stripMargin
    AExecutablePath.toFile(agentDirectory / "config" / "executables").writeExecutable(AScript)
    BExecutablePath.toFile(agentDirectory / "config" / "executables").writeExecutable(AScript)
    super.beforeAll()
  }

  "ShutDown" in {
    implicit val actorSystem = newAgentActorSystem("TerminateTest")
    closer onClose actorSystem.terminate()

    val client = AgentClient(agentUri = agent.localUri)
    client.login(Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD"))) await 99.s
    client.commandExecute(RegisterAsMaster) await 99.s

    val eventCollector = newEventCollector(agent.injector)

    val orderIds = for (i <- 0 until 3) yield OrderId(s"TEST-ORDER-$i")
    (for (orderId <- orderIds) yield
      client.commandExecute(AttachOrder(
        Order(
          orderId,
          SimpleTestWorkflow.id,
          Order.Ready,
          Map("a" -> "A")),
        TestAgentRefPath,
        fileBasedSigner.sign(SimpleTestWorkflow)))
    ) await 99.s

    val whenStepEnded: Future[Seq[OrderEvent.OrderProcessed]] =
      Future.sequence(
        for (orderId <- orderIds) yield
          eventCollector.whenKeyedEvent[OrderEvent.OrderProcessed](EventRequest.singleClass(timeout = Some(90.s)), orderId)
            .runToFuture: Future[OrderEvent.OrderProcessed])
    sleep(2.s)
    assert(!whenStepEnded.isCompleted)

    client.commandExecute(ShutDown(sigkillProcessesAfter = Some(0.seconds))).await(99.s).orThrow
    val stepEnded = whenStepEnded await 99.s
    assert(stepEnded forall { _.outcome.asInstanceOf[Outcome.Undisrupted].isFailed })
    agent.terminated await 99.s
    client.close()
  }
}

object TerminateTest {
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
