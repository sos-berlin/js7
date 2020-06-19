package js7.tests.https

import js7.base.BuildInfo
import js7.base.time.ScalaTime._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.data.ControllerCommand
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.WorkflowPath
import monix.execution.Scheduler.Implicits.global

/**
  * @author Joacim Zschimmer
  */
private[https] trait ControllerHttpsStandardTests extends HttpsTestBase
{
  override protected def waitUntilReady = false

  "Agents use https://" in {
    // Referencing agents implicitly starts them (before controller)
    assert(agents.forall(_.localUri.string startsWith "https://"))
  }

  "ClusterCoupled" in {
    if (useCluster) backupController
    controller.waitUntilReady()
    controller.eventWatch.await[ClusterCoupled]()
  }

  "Controller uses https://" in {
    assert(controller.localUri.string startsWith "https://")
  }

  "overview" in {
    val overview = controllerApi.overview await 99.s
    assert(overview.buildId == BuildInfo.buildId)
  }

  "Login" in {
    controllerApi.login() await 99.s
  }

  "Run a job" in {
    controllerApi.addOrder(FreshOrder(OrderId("TEST"), WorkflowPath("/TEST-WORKFLOW"))) await 99.s
    controller.eventWatch.await[OrderFinished]()
  }

  "ShutDown" in {
    controllerApi.executeCommand(ControllerCommand.ShutDown()) await 99.s
    controllerApi.clearSession()  // To avoid automatic logoff because Controller is terminating now.
    controller.terminated await 99.s
  }
}
