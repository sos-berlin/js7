package js7.tests.cluster.controller

import js7.base.problem.Checked.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost, ClusterWatchRegistered}
import js7.data.controller.ControllerCommand.ClusterAppointNodes
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.NumberValue
import js7.tests.cluster.controller.ControllerClusterTester.*
import monix.execution.Scheduler.Implicits.traced

final class PassiveLostControllerClusterTest extends ControllerClusterTester:
  override protected def configureClusterNodes = false

  "Passive lost" in:
    withControllerAndBackup() { (primary, _, backup, _, clusterSetting) =>
      val primaryController = primary.newController()

      var backupController = backup.newController()

      primaryController.api
        .executeCommand(ClusterAppointNodes(clusterSetting.idToUri, clusterSetting.activeId))
        .await(99.s).orThrow
      primaryController.eventWatch.await[ClusterCoupled]()

      val firstOrderId = OrderId("🔺")
      locally:
        primaryController.addOrderBlocking(FreshOrder(firstOrderId, TestWorkflow.id.path,
          Map("sleep" -> NumberValue(1))))
        primaryController.eventWatch.await[OrderProcessingStarted](_.key == firstOrderId)
        backupController.eventWatch.await[OrderProcessingStarted](_.key == firstOrderId)

      primaryController.eventWatch.await[ClusterWatchRegistered]()

      for orderId <- Array(OrderId("🔸"), OrderId("🔶")) do
        backupController.terminate(dontNotifyActiveNode = true) await 99.s

        val passiveLost = primaryController.eventWatch.await[ClusterPassiveLost]().head.eventId

        primaryController.eventWatch.await[OrderFinished](_.key == firstOrderId, after = passiveLost)

        backupController = backup.newController()
        primaryController.eventWatch.await[ClusterCoupled]().head.eventId

        primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        primaryController.eventWatch.await[OrderFinished](_.key == orderId, after = passiveLost)
        backupController.eventWatch.await[OrderFinished](_.key == orderId, after = passiveLost)

      primaryController.stop.await(99.s)
      backupController.stop.await(99.s)
    }
