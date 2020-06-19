package js7.tests.https

import js7.base.BuildInfo
import js7.base.time.ScalaTime._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.WorkflowPath
import js7.master.data.MasterCommand
import monix.execution.Scheduler.Implicits.global

/**
  * @author Joacim Zschimmer
  */
private[https] trait MasterHttpsStandardTests extends HttpsTestBase
{
  override protected def waitUntilReady = false

  "Agents use https://" in {
    // Referencing agents implicitly starts them (before master)
    assert(agents.forall(_.localUri.string startsWith "https://"))
  }

  "ClusterCoupled" in {
    if (useCluster) backupMaster
    master.waitUntilReady()
    master.eventWatch.await[ClusterCoupled]()
  }

  "Controller uses https://" in {
    assert(master.localUri.string startsWith "https://")
  }

  "overview" in {
    val overview = masterApi.overview await 99.s
    assert(overview.buildId == BuildInfo.buildId)
  }

  "Login" in {
    masterApi.login() await 99.s
  }

  "Run a job" in {
    masterApi.addOrder(FreshOrder(OrderId("TEST"), WorkflowPath("/TEST-WORKFLOW"))) await 99.s
    master.eventWatch.await[OrderFinished]()
  }

  "ShutDown" in {
    masterApi.executeCommand(MasterCommand.ShutDown()) await 99.s
    masterApi.clearSession()  // To avoid automatic logoff because Master is terminating now.
    master.terminated await 99.s
  }
}
