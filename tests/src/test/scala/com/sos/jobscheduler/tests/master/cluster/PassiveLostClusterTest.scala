package js7.tests.master.cluster

import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost}
import js7.data.cluster.{ClusterEvent, ClusterNodeId}
import js7.data.event.KeyedEvent.NoKey
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.master.data.MasterCommand.ClusterAppointNodes
import js7.tests.master.cluster.MasterClusterTester._
import monix.execution.Scheduler.Implicits.global

final class PassiveLostClusterTest extends MasterClusterTester
{
  override protected def configureClusterNodes = false

  "Passive lost" in {
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      val primaryMaster = primary.startMaster(httpPort = Some(primaryHttpPort)) await 99.s
      var backupMaster = backup.startMaster(httpPort = Some(backupHttpPort)) await 99.s

      primaryMaster.executeCommandAsSystemUser(
        ClusterAppointNodes(
          Map(
            ClusterNodeId("Primary") -> primaryMaster.localUri,
            ClusterNodeId("Backup") -> backupMaster.localUri),
          ClusterNodeId("Primary"))
      ).await(99.s).orThrow
      primaryMaster.eventWatch.await[ClusterEvent.ClusterCoupled]()

      val firstOrderId = OrderId("🔺")
      locally {
        primaryMaster.addOrderBlocking(FreshOrder(firstOrderId, TestWorkflow.id.path))
        primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == firstOrderId)
        backupMaster.eventWatch.await[OrderProcessingStarted](_.key == firstOrderId)
      }

      for (orderId <- Array(OrderId("🔸"), OrderId("🔶"))) {
        backupMaster.terminate() await 99.s
        val passiveLost = primaryMaster.eventWatch.await[ClusterPassiveLost]().head.eventId

        primaryMaster.eventWatch.await[OrderFinished](_.key == firstOrderId, after = passiveLost)

        backupMaster = backup.startMaster(httpPort = Some(backupHttpPort)) await 99.s
        primaryMaster.eventWatch.await[ClusterCoupled]().head.eventId

        primaryMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        primaryMaster.eventWatch.await[OrderFinished](_.key == orderId, after = passiveLost)
        backupMaster.eventWatch.await[OrderFinished](_.key == orderId, after = passiveLost)
      }

      primaryMaster.terminate() await 99.s
      backupMaster.terminate() await 99.s
    }
  }
}
