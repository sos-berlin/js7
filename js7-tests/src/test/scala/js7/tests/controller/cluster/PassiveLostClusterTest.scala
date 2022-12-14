package js7.tests.controller.cluster

import js7.base.problem.Checked.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.cluster.ClusterEvent
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost}
import js7.data.controller.ControllerCommand.ClusterAppointNodes
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.tests.controller.cluster.ControllerClusterTester.*
import monix.execution.Scheduler.Implicits.traced

final class PassiveLostClusterWithLegacyClusterWatchTest extends PassiveLostClusterTest {
  override protected val useLegacyServiceClusterWatch = true
}

class PassiveLostClusterTest extends ControllerClusterTester
{
  override protected def configureClusterNodes = false

  "Passive lost" in {
    withControllerAndBackup() { (primary, backup, clusterSetting) =>
      val primaryController = primary.startController(httpPort = Some(primaryControllerPort)) await 99.s
      var backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s

      primaryController.executeCommandForTest(
        ClusterAppointNodes(clusterSetting.idToUri, clusterSetting.activeId, clusterSetting.clusterWatches)
      ).orThrow
      primaryController.eventWatch.await[ClusterEvent.ClusterCoupled]()

      val firstOrderId = OrderId("🔺")
      locally {
        primaryController.addOrderBlocking(FreshOrder(firstOrderId, TestWorkflow.id.path))
        primaryController.eventWatch.await[OrderProcessingStarted](_.key == firstOrderId)
        backupController.eventWatch.await[OrderProcessingStarted](_.key == firstOrderId)
      }

      for (orderId <- Array(OrderId("🔸"), OrderId("🔶"))) {
        backupController.terminate() await 99.s
        val passiveLost = primaryController.eventWatch.await[ClusterPassiveLost]().head.eventId

        primaryController.eventWatch.await[OrderFinished](_.key == firstOrderId, after = passiveLost)

        backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s
        primaryController.eventWatch.await[ClusterCoupled]().head.eventId

        primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        primaryController.eventWatch.await[OrderFinished](_.key == orderId, after = passiveLost)
        backupController.eventWatch.await[OrderFinished](_.key == orderId, after = passiveLost)
      }

      primaryController.terminate() await 99.s
      backupController.terminate() await 99.s
    }
  }
}
