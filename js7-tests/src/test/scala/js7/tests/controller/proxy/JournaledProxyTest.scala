package js7.tests.controller.proxy

import js7.base.auth.{UserAndPassword, UserId}
import js7.base.eventbus.StandardEventBus
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.RunningController
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.data.ControllerState
import js7.data.agent.AgentRefPath
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed}
import js7.data.order.{FreshOrder, Order, OrderId, Outcome}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.proxy.javaapi.JCredentials
import js7.proxy.javaapi.data.JHttpsConfig
import js7.proxy.{JournaledProxy, JournaledStateEventBus, ProxyEvent}
import js7.tests.controller.proxy.JournaledProxyTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.jdk.FutureConverters._

final class JournaledProxyTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val fileBased = workflow :: Nil
  protected val agentRefPaths = agentRefPath :: Nil

  override def beforeAll() = {
    super.beforeAll()
    (directoryProvider.controller.configDir / "private" / "private.conf") ++= """
      |js7.auth.users.TEST-USER = "plain:TEST-PASSWORD"
      |""".stripMargin
    directoryProvider.agents.head.writeExecutable(ExecutablePath("/test.cmd"), script(1.s))
  }

  "JournaledProxy[JControllerState]" in {
    directoryProvider.run { (controller, _) =>
      val controllerApiResource = AkkaHttpControllerApi.separateAkkaResource(controller.localUri, Some(userAndPassword), name = "JournaledProxy")
      val proxyEventBus = new StandardEventBus[ProxyEvent]
      val controllerEventBus = new JournaledStateEventBus[ControllerState]
      val proxy = JournaledProxy.start[ControllerState](controllerApiResource, proxyEventBus.publish, controllerEventBus.publish) await 99.s
      try {
        val whenProcessed = controllerEventBus.when[OrderProcessed].runToFuture
        val whenFinished = controllerEventBus.when[OrderFinished.type].runToFuture
        controller.addOrder(FreshOrder(OrderId("🔺"), workflow.id.path)).runAsyncAndForget

        val processed = whenProcessed.await(99.s)
        assert(processed.stampedEvent.value.event == OrderProcessed(Outcome.succeeded))
        assert(processed.state.idToOrder(OrderId("🔺")).state == Order.Processed)

        whenFinished await 99.s  // Await order termination before shutting down the JS7
      } finally proxy.stop await 99.s
    }
  }

  "JControllerProxy" in {
    directoryProvider.runAgents() { _ =>
      val port = findFreeTcpPort()
      lazy val controller: RunningController = directoryProvider.startController(httpPort = Some(port)).await(99.s)
      val startController: Runnable = () => controller
      val uri = s"http://127.0.0.1:$port"
      JControllerProxyTester.start(uri, JCredentials.JUserAndPassword(userAndPassword), JHttpsConfig.empty, startController)
        .asScala
        .flatMap { tester =>
          tester.test()
          tester.stop().asScala
        }
        .await(99.s)
    }
  }
}

object JournaledProxyTest
{
  private val agentRefPath = AgentRefPath("/AGENT")
  private val workflow = WorkflowParser.parse(
    WorkflowPath("/WORKFLOW") ~ "INITIAL",
    s"""
      define workflow {
        execute executable="/test.cmd", agent="AGENT";
      }"""
  ).orThrow

  private val userAndPassword = UserAndPassword(UserId("TEST-USER") -> SecretString("TEST-PASSWORD"))
}
