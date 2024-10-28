package js7.tests.cluster.controller

import js7.base.problem.Checked.*
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.SetOnce
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost, ClusterWatchRegistered}
import js7.data.controller.ControllerCommand.ClusterAppointNodes
import js7.data.event.EventId
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.NumberValue
import js7.tests.cluster.controller.ControllerClusterTester.*

final class PassiveLostControllerClusterTest extends ControllerClusterTester:

  override protected def configureClusterNodes = false

  "Passive lost" in:
    withControllerAndBackup(): (primary, _, backup, _, clusterSetting) =>
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
      val firstPassiveLost = SetOnce[EventId]

      for orderId <- (1 to 5).map(i => OrderId(s"🔶$i")) do
        val eventId = primaryController.eventWatch.lastAddedEventId
        backupController.terminate(dontNotifyActiveNode = true).await(99.s)

        val passiveLost = primaryController.eventWatch.await[ClusterPassiveLost](after = eventId)
          .head.eventId
        firstPassiveLost.trySet(passiveLost)

        primaryController.eventWatch
          .await[OrderFinished](_.key == firstOrderId, after = firstPassiveLost.orThrow)

        backupController = backup.newController()
        primaryController.eventWatch.await[ClusterCoupled](after = passiveLost).head.eventId

        primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        primaryController.eventWatch.await[OrderFinished](_.key == orderId, after = passiveLost)
        backupController.eventWatch.await[OrderFinished](_.key == orderId, after = passiveLost)
      end for

      primaryController.stop.await(99.s)
      backupController.stop.await(99.s)
