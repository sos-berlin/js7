package com.sos.jobscheduler.tests.master.proxy

import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderFinished, OrderProcessed}
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.master.data.MasterState
import com.sos.jobscheduler.proxy.javaapi.JCredentials
import com.sos.jobscheduler.proxy.{JournaledProxy, ProxyEventBus}
import com.sos.jobscheduler.tests.master.proxy.JournaledProxyTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.script
import com.sos.jobscheduler.tests.testenv.DirectoryProviderForScalaTest
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
      |jobscheduler.auth.users.TEST-USER = "plain:TEST-PASSWORD"
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

        whenFinished await 99.s  // Await order termination before shutting down the JobScheduler
      } finally proxy.stop() await 99.s
    }
  }

  "JMasterProxy" in {
    directoryProvider.run { (master, _) =>
      JMasterProxyTester.start(master.localUri.string, JCredentials.JUserAndPassword(userAndPassword))
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
