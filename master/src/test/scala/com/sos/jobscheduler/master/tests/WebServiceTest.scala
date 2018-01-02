package com.sos.jobscheduler.master.tests

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.FileUtils.temporaryDirectory
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.TestWorkflowScript
import com.sos.jobscheduler.data.workflow.{NodeId, NodeKey, WorkflowGraph, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.client.{AkkaHttpClient, HttpMasterApi}
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.order.MasterOrderKeeper
import com.sos.jobscheduler.master.tests.WebServiceTest._
import com.sos.jobscheduler.shared.workflow.script.WorkflowScriptToGraph.workflowScriptToGraph
import io.circe.syntax.EncoderOps
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class WebServiceTest extends FreeSpec with BeforeAndAfterAll {

  private lazy val env = new TestEnvironment(agentPaths = Nil, temporaryDirectory / "WebServiceTest")
  private lazy val httpClient = new AkkaHttpClient.StandAlone
  private var master: RunningMaster = _
  private var api: HttpMasterApi = _

  override def beforeAll() = {
    super.beforeAll()
    //env.xmlFile(TestAgentPath).xml = <agent uri="http://0.0.0.0:0"/>
    env.jsonFile(TestWorkflowPath).contentString = TestWorkflowScript.asJson.toPrettyString
    val runningMaster = RunningMaster(MasterConfiguration.forTest(configAndData = env.masterDir)) await 99.s
    master = runningMaster
    for (t ← master.terminated.failed) logger.error(t.toStringWithCauses, t)
    api = new HttpMasterApi(master.localUri.toString, httpClient)
  }

  override def afterAll() = {
    if (master != null) master.close()
    env.close()
    httpClient.close()
    super.afterAll()
  }

  "AddOrderIfNew" in {
    master.orderKeeper ! MasterOrderKeeper.Input.SuspendDetaching  // Suspend order processing
    api.executeCommand(MasterCommand.AddOrderIfNew(adHocOrder)) await 10.s
  }

  "overview" in {
    assert(api.overview.await(99.s).version == BuildInfo.buildVersion)
  }

  "ordersOverview" in {
    assert(api.ordersOverview.await(99.s).orderCount == 1)
  }

  "orders" in {
    assert(api.orders.await(99.s).value == List(adHocOrder))
  }

  "workflow" in {
    assert(api.workflows.await(99.s).value == List(WorkflowGraph.Named(TestWorkflowPath, workflowScriptToGraph(TestWorkflowScript))))
  }
}

private object WebServiceTest {
  private val logger = Logger(getClass)
  private val TestWorkflowPath = WorkflowPath("/WORKFLOW")
  private val adHocOrder = Order(OrderId("ORDER-ID"), NodeKey(TestWorkflowPath, NodeId("100")), Order.StartNow)
}
