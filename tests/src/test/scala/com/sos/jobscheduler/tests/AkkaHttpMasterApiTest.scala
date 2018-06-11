package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.FileUtils.temporaryDirectory
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.{TestWorkflow, TestWorkflowNotation}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.tests.TestEnvironment
import com.sos.jobscheduler.tests.AkkaHttpMasterApiTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpMasterApiTest extends FreeSpec with BeforeAndAfterAll {

  private lazy val env = new TestEnvironment(agentPaths = Nil, temporaryDirectory / "AkkaHttpMasterApiTest")
  private var master: RunningMaster = _
  private var api: AkkaHttpMasterApi = _

  override def beforeAll() = {
    super.beforeAll()
    env.writeTxt(TestWorkflowId.path, TestWorkflowNotation)
    env.masterPrivateConf.contentString = """jobscheduler.auth.users.TEST-USER = "plain:TEST-PASSWORD" """
    master = RunningMaster(MasterConfiguration.forTest(configAndData = env.masterDir)) await 99.s
    for (t ← master.terminated.failed) logger.error(t.toStringWithCauses, t)
    api = new AkkaHttpMasterApi(master.localUri)
  }

  override def afterAll() = {
    if (master != null) master.close()
    if (api != null) api.close()
    env.close()
    super.afterAll()
  }

  "login" in {
    api.login(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s
  }

  "POST order" in {
    assert(api.addOrder(FreshOrder(OrderId("ORDER-ID"), TestWorkflowId.path)).await(99.s) == true)
    assert(api.addOrder(FreshOrder(OrderId("ORDER-ID"), TestWorkflowId.path)).await(99.s) == false)  // Duplicate
  }

  "overview" in {
    assert(api.overview.await(99.s).version == BuildInfo.buildVersion)
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

  //"AddOrderIfNew" in {
  //  // Deprecated
  //  val freshOrder = FreshOrder(OrderId("ORDER-2"), TestWorkflowId.path)
  //  api.executeCommand(MasterCommand.AddOrderIfNew.fromFreshOrder(freshOrder)) await 10.s
  //  assert(api.orders.await(99.s).value.toSet == Set(TestOrder, freshOrder.toOrder(VersionId("(initial)"))))
  //}
}

private object AkkaHttpMasterApiTest {
  private val logger = Logger(getClass)
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") % "(initial)"
  private val TestOrder = Order(OrderId("ORDER-ID"), TestWorkflowId, Order.Fresh.StartImmediately)
}
