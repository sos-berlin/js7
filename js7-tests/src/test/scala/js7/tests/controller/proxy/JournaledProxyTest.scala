package js7.tests.controller.proxy

import js7.base.auth.{UserAndPassword, UserId}
import js7.base.eventbus.StandardEventBus
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.utils.Lazy
import js7.common.configutils.Configs._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.data.ControllerState
import js7.data.agent.AgentRefPath
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed}
import js7.data.order.{FreshOrder, Order, OrderId, Outcome}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.proxy.javaapi.data.JHttpsConfig
import js7.proxy.javaapi.{JAdmission, JCredentials}
import js7.proxy.{ControllerProxy, JournaledStateEventBus, ProxyEvent}
import js7.tests.controller.proxy.JournaledProxyTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.jdk.CollectionConverters._

final class JournaledProxyTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  override protected def controllerConfig = config"""
    js7.auth.users {
      Proxy {
        password = "plain:PROXYS-PASSWORD-FOR-PRIMARY"
      }
    }
    """

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
      val proxy = ControllerProxy.start(controllerApiResource :: Nil, proxyEventBus.publish, controllerEventBus.publish)
        .await(99.s)
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
      val controller = Lazy { directoryProvider.startController(httpPort = Some(port)).await(99.s) }
      try {
        val admissions = List(JAdmission.of(s"http://127.0.0.1:$port", primaryCredentials)).asJava
        JControllerProxyTester.run(admissions, JHttpsConfig.empty, () => controller())
      } finally
        for (controller <- controller) {
          controller.terminate() await 99.s
          controller.close()
        }
    }
  }

  "JControllerProxy with Flux" in {
    directoryProvider.runAgents() { _ =>
      val port = findFreeTcpPort()
      val controller = Lazy { directoryProvider.startController(httpPort = Some(port)).await(99.s) }
      try {
        val admissions = List(JAdmission.of(s"http://127.0.0.1:$port", primaryCredentials)).asJava
        val tester = new JControllerFluxTester(admissions, JHttpsConfig.empty)
        tester.test(() => controller())
        tester.testReusage()
        tester.close()
      } finally
        for (controller <- controller) {
          controller.terminate() await 99.s
          controller.close()
        }
    }
  }
}

object JournaledProxyTest
{
  private[proxy] val agentRefPath = AgentRefPath("/AGENT")
  private[proxy] val workflow = WorkflowParser.parse(
    WorkflowPath("/WORKFLOW") ~ "INITIAL",
    s"""
      define workflow {
        execute executable="/test.cmd", agent="AGENT", taskLimit=10;
      }"""
  ).orThrow

  private[proxy] val userAndPassword = UserAndPassword(UserId("Proxy") -> SecretString("PROXYS-PASSWORD-FOR-PRIMARY"))
  private[proxy] val primaryCredentials = JCredentials.JUserAndPassword(userAndPassword)
}
