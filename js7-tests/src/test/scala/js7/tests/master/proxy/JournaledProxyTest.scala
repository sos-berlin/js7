package js7.tests.master.proxy

import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.data.agent.AgentRefPath
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed}
import js7.data.order.{FreshOrder, Order, OrderId, Outcome}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.master.client.AkkaHttpMasterApi
import js7.master.data.MasterState
import js7.proxy.javaapi.JCredentials
import js7.proxy.javaapi.data.JHttpsConfig
import js7.proxy.{JournaledProxy, ProxyEventBus}
import js7.tests.master.proxy.JournaledProxyTest._
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
    (directoryProvider.master.configDir / "private" / "private.conf") ++= """
      |js7.auth.users.TEST-USER = "plain:TEST-PASSWORD"
      |""".stripMargin
    directoryProvider.agents.head.writeExecutable(ExecutablePath("/test.cmd"), script(1.s))
  }

  "JournaledProxy[JMasterState]" in {
    directoryProvider.run { (master, _) =>
      val masterApiResource = AkkaHttpMasterApi.separateAkkaResource(master.localUri, Some(userAndPassword), name = "JournaledProxy")
      val eventBus = new ProxyEventBus[MasterState]
      //val eventBus = new ProxyEventBus[JMasterState]
      val proxy = JournaledProxy.start(masterApiResource, eventBus.publish) await 99.s
      try {
        val whenProcessed = eventBus.when[OrderProcessed].runToFuture
        val whenFinished = eventBus.when[OrderFinished.type].runToFuture
        master.addOrder(FreshOrder(OrderId("🔺"), workflow.id.path)).runAsyncAndForget

        val processed = whenProcessed.await(99.s)
        assert(processed.stampedEvent.value.event == OrderProcessed(Outcome.succeeded))
        assert(processed.state.idToOrder(OrderId("🔺")).state == Order.Processed)

        whenFinished await 99.s  // Await order termination before shutting down the JS7
      } finally proxy.stop() await 99.s
    }
  }

  "JMasterProxy" in {
    directoryProvider.run { (master, _) =>
      JMasterProxyTester.start(master.localUri.string, JCredentials.JUserAndPassword(userAndPassword), JHttpsConfig.empty)
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
