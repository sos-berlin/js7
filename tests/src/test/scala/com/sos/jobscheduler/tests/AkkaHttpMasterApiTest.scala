package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.BuildInfo
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Closer.syntax.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.master.data.MasterCommand.AddOrder
import com.sos.jobscheduler.tests.AkkaHttpMasterApiTest._
import com.sos.jobscheduler.tests.testenv.MasterAgentForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpMasterApiTest extends AnyFreeSpec with MasterAgentForScalaTest
{
  protected val agentRefPaths = Nil
  protected val fileBased = TestWorkflow :: Nil

  private lazy val api = new AkkaHttpMasterApi(master.localUri, Some(userAndPassword), actorSystem = master.actorSystem)
    .closeWithCloser

  override def beforeAll() = {
    directoryProvider.master.configDir / "private" / "private.conf" ++= """
        |jobscheduler.auth.users.TEST-USER = "plain:TEST-PASSWORD"
        |""".stripMargin
    super.beforeAll()
  }

  "login" in {
    assert(master.sessionRegister.count.await(99.s) == 1)
    api.login() await 99.s
    assert(master.sessionRegister.count.await(99.s) == 2)
    api.login() await 99.s
    assert(master.sessionRegister.count.await(99.s) == 2)
  }

  "POST order" in {
    assert(api.addOrder(FreshOrder(TestOrder.id, TestWorkflow.path)).await(99.s) == true)
    assert(api.addOrder(FreshOrder(TestOrder.id, TestWorkflow.path)).await(99.s) == false)  // Duplicate
  }

  "MasterCommand.AddOrder" in {
    assert(api.executeCommand(AddOrder(FreshOrder(TestOrder.id, TestWorkflow.path))).await(99.s) ==
      AddOrder.Response(ignoredBecauseDuplicate = true))
    assert(api.executeCommand(AddOrder(FreshOrder(SecondOrder.id, TestWorkflow.path))).await(99.s) ==
      AddOrder.Response(ignoredBecauseDuplicate = false))
  }

  "overview" in {
    assert(api.overview.await(99.s).version == BuildInfo.prettyVersion)
  }

  "ordersOverview" in {
    assert(api.ordersOverview.await(99.s).count == 2)
  }

  "orders" in {
    assert(api.orders.await(99.s).map(_.toSet) == Right(Set(TestOrder, SecondOrder)))
  }

  "workflow" in {
    assert(api.workflows.await(99.s) == Right(List(TestWorkflow)))
  }

  "logout" in {
    assert(master.sessionRegister.count.await(99.s) == 2)
    api.logout() await 99.s
    assert(master.sessionRegister.count.await(99.s) == 1)
  }

  "resource" in {
    AkkaHttpMasterApi.separateAkkaResource(master.localUri, userAndPassword = Some(userAndPassword))
      .use(api => Task {
        api.login() await 99.s
        assert(master.sessionRegister.count.await(99.s) == 2)
        assert(api.orders.await(99.s).map(_.toSet) == Right(Set(TestOrder, SecondOrder)))
      })
      .map(_ => master.sessionRegister.count.await(99.s) == 1)
      .await(99.s)
  }
}

private object AkkaHttpMasterApiTest
{
  private val userAndPassword = UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD"))
  private val TestWorkflow = Workflow.of(WorkflowPath("/WORKFLOW") ~ "INITIAL",
    Execute(WorkflowJob(AgentRefPath("/MISSING"), ExecutablePath("/MISSING"))))
  private val TestOrder = Order(OrderId("ORDER-ID"), TestWorkflow.id, Order.Fresh.StartImmediately)
  private val SecondOrder = Order(OrderId("SECOND-ORDER"), TestWorkflow.id, Order.Fresh.StartImmediately)
}
