package js7.tests.https

import js7.base.BuildInfo
import js7.base.auth.UserAndPassword
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

  override def afterAll() = {
    controllerApi.login() await 99.s
    controllerApi.executeCommand(ControllerCommand.ShutDown()) await 99.s
    controllerApi.clearSession()  // To avoid automatic logoff because Controller is terminating now
    controller.terminated await 99.s
    super.afterAll()
  }

  "Controller and Agents use https://" in {
    assert(controller.localUri.string startsWith "https://")
    // Referencing agents implicitly starts them (before controller)
    assert(agents.forall(_.localUri.string startsWith "https://"))
  }

  if (useCluster)
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

  protected def addTestsForCredentials(credentials: Option[UserAndPassword]): Unit = {
    "overview" in {
      val overview = controllerApi.overview await 99.s
      assert(overview.buildId == BuildInfo.buildId)
    }

    "Login" in {
      controllerApi.login_(credentials) await 99.s
    }

    "Run a job" in {
      controllerApi.addOrder(FreshOrder(OrderId("TEST"), WorkflowPath("/TEST-WORKFLOW"))) await 99.s
      controller.eventWatch.await[OrderFinished]()
    }

    "logout" in {
      controllerApi.logout() await 99.s
    }
  }

  "Standard tests" - {
    addTestsForCredentials(standardUserAndPassword)
  }
}
