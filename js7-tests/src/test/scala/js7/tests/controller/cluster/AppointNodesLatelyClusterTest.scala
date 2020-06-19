package js7.tests.controller.cluster

import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.controller.data.ControllerCommand.ClusterAppointNodes
import js7.core.event.journal.files.JournalFiles.listJournalFiles
import js7.data.cluster.{ClusterEvent, ClusterNodeId}
import js7.data.event.EventId
import js7.data.order.OrderEvent.{OrderFinished, OrderStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.tests.controller.cluster.ControllerClusterTester._
import monix.execution.Scheduler.Implicits.global

final class AppointNodesLatelyClusterTest extends ControllerClusterTester
{
  override protected def configureClusterNodes = false

  "ClusterAppointNodes command after first journal file has been deleted" in {
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withControllerAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      primary.runController(httpPort = Some(primaryHttpPort)) { primaryController =>
        val orderId = OrderId("🔺")
        primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        primaryController.eventWatch.await[OrderStarted](_.key == orderId)
      }

      primary.runController(httpPort = Some(primaryHttpPort)) { primaryController =>
        assert(listJournalFiles(primary.controller.dataDir / "state" / "controller").head.afterEventId > EventId.BeforeFirst)

        val backupController = backup.startController(httpPort = Some(backupHttpPort)) await 99.s
        primaryController.executeCommandAsSystemUser(
          ClusterAppointNodes(
            Map(
              ClusterNodeId("Primary") -> primaryController.localUri,
              ClusterNodeId("Backup") -> backupController.localUri),
            ClusterNodeId("Primary"))
        ).await(99.s).orThrow
        primaryController.eventWatch.await[ClusterEvent.ClusterCoupled]()

        val orderId = OrderId("🔸")
        primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        primaryController.eventWatch.await[OrderFinished](_.key == orderId)
        backupController.eventWatch.await[OrderFinished](_.key == orderId)

        primaryController.terminate() await 99.s
        backupController.terminate() await 99.s
      }
    }
  }
}
