package js7.tests.controller.cluster

import js7.base.problem.Checked.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost, ClusterWatchRegistered}
import js7.data.controller.ControllerCommand.ClusterAppointNodes
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.NumberValue
import js7.tests.controller.cluster.ControllerClusterTester.*
import monix.execution.Scheduler.Implicits.traced

final class PassiveLostClusterTest extends ControllerClusterTester
{
  override protected def configureClusterNodes = false

  "Passive lost" in {
    withControllerAndBackup() { (primary, backup, clusterSetting) =>
      val primaryController = primary.newController(httpPort = Some(primaryControllerPort))

      var backupController = backup.newController(httpPort = Some(backupControllerPort))

      primaryController.executeCommandForTest(
        ClusterAppointNodes(clusterSetting.idToUri, clusterSetting.activeId)
      ).orThrow
      primaryController.eventWatch.await[ClusterCoupled]()

      val firstOrderId = OrderId("🔺")
      locally {
        primaryController.addOrderBlocking(FreshOrder(firstOrderId, TestWorkflow.id.path,
          Map("sleep" -> NumberValue(1))))
        primaryController.eventWatch.await[OrderProcessingStarted](_.key == firstOrderId)
        backupController.eventWatch.await[OrderProcessingStarted](_.key == firstOrderId)
      }

      primaryController.eventWatch.await[ClusterWatchRegistered]()

      for (orderId <- Array(OrderId("🔸"), OrderId("🔶"))) {
        backupController.terminate(dontNotifyActiveNode = true) await 99.s
        backupController.close()

        val passiveLost = primaryController.eventWatch.await[ClusterPassiveLost]().head.eventId

        primaryController.eventWatch.await[OrderFinished](_.key == firstOrderId, after = passiveLost)

        backupController = backup.newController(httpPort = Some(backupControllerPort))
        primaryController.eventWatch.await[ClusterCoupled]().head.eventId

        primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        primaryController.eventWatch.await[OrderFinished](_.key == orderId, after = passiveLost)
        backupController.eventWatch.await[OrderFinished](_.key == orderId, after = passiveLost)
      }

      primaryController.close()
      backupController.close()
    }
  }
}
