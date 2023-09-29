package js7.tests.https

import js7.base.BuildInfo
import js7.base.auth.UserAndPassword
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.WorkflowPath
import monix.execution.Scheduler.Implicits.traced

/**
  * @author Joacim Zschimmer
  */
private[https] trait ControllerHttpsStandardTests extends HttpsTestBase
{
  override protected def waitUntilReady = false

  "Controller and Agents use https://" in {
    assert(controller.localUri.string startsWith "https://")
    // Referencing agents implicitly starts them (before controller)
    assert(agents.forall(_.localUri.string startsWith "https://"))
  }

  if useCluster then
    "ClusterCoupled" in {
      backupController
      controller.waitUntilReady()
      controller.eventWatch.await[ClusterCoupled]()
    }
  else
    "waitUntilReady" in {
      controller.waitUntilReady()
    }

  "Controller uses https://" in {
    assert(controller.localUri.string startsWith "https://")
  }

  protected def addTestsForCredentials(
    credentials: Option[UserAndPassword] = standardUserAndPassword)
  : Unit =
  "Standard tests" - {
    "overview" in {
      val overview = httpControllerApi.overview await 99.s
      assert(overview.buildId == BuildInfo.buildId)
    }

    "Login" in {
      httpControllerApi.login_(credentials) await 99.s
    }

    "Run a job" in {
      httpControllerApi.addOrder(FreshOrder(OrderId("TEST"), WorkflowPath("TEST-WORKFLOW"))) await 99.s
      controller.eventWatch.await[OrderFinished]()
    }

    "logout" in {
      httpControllerApi.logout() await 99.s
    }
  }
}
