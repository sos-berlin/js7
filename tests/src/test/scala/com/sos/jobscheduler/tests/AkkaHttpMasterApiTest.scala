package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.tests.AkkaHttpMasterApiTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpMasterApiTest extends FreeSpec with DirectoryProviderForScalaTest {

  protected val agentRefPaths = Nil
  protected val fileBased = TestWorkflow :: Nil

  private lazy val api = new AkkaHttpMasterApi(master.localUri)
    .closeWithCloser

  override def beforeAll() = {
    directoryProvider.master.config / "private" / "private.conf" ++= """
        |jobscheduler.auth.users.TEST-USER = "plain:TEST-PASSWORD"
        |""".stripMargin
    super.beforeAll()
  }

  "login" in {
    api.login(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s
  }

  "POST order" in {
    assert(api.addOrder(FreshOrder(OrderId("ORDER-ID"), TestWorkflow.path)).await(99.s) == true)
    assert(api.addOrder(FreshOrder(OrderId("ORDER-ID"), TestWorkflow.path)).await(99.s) == false)  // Duplicate
  }

  "overview" in {
    assert(api.overview.await(99.s).version == BuildInfo.prettyVersion)
  }

  "ordersOverview" in {
    assert(api.ordersOverview.await(99.s).count == 1)
  }

  "orders" in {
    assert(api.orders.await(99.s).value == List(TestOrder))
  }

  "workflow" in {
    assert(api.workflows.await(99.s).value == List(TestWorkflow))
  }
}

private object AkkaHttpMasterApiTest
{
  private val TestWorkflow = Workflow.of(WorkflowPath("/WORKFLOW") ~ "INITIAL",
    Execute(WorkflowJob(AgentRefPath("/MISSING"), ExecutablePath("/MISSING"))))
  private val TestOrder = Order(OrderId("ORDER-ID"), TestWorkflow.id, Order.Fresh.StartImmediately)
}
