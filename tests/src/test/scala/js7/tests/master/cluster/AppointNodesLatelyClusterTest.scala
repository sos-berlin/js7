package js7.tests.master.cluster

import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.core.event.journal.files.JournalFiles.listJournalFiles
import js7.data.cluster.{ClusterEvent, ClusterNodeId}
import js7.data.event.EventId
import js7.data.order.OrderEvent.{OrderFinished, OrderStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.master.data.MasterCommand.ClusterAppointNodes
import js7.tests.master.cluster.MasterClusterTester._
import monix.execution.Scheduler.Implicits.global

final class AppointNodesLatelyClusterTest extends MasterClusterTester
{
  override protected def configureClusterNodes = false

  "ClusterAppointNodes command after first journal file has been deleted" in {
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
        val orderId = OrderId("🔺")
        primaryMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        primaryMaster.eventWatch.await[OrderStarted](_.key == orderId)
      }

      primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
        assert(listJournalFiles(primary.master.dataDir / "state" / "master").head.afterEventId > EventId.BeforeFirst)

        val backupMaster = backup.startMaster(httpPort = Some(backupHttpPort)) await 99.s
        primaryMaster.executeCommandAsSystemUser(
          ClusterAppointNodes(
            Map(
              ClusterNodeId("Primary") -> primaryMaster.localUri,
              ClusterNodeId("Backup") -> backupMaster.localUri),
            ClusterNodeId("Primary"))
        ).await(99.s).orThrow
        primaryMaster.eventWatch.await[ClusterEvent.ClusterCoupled]()

        val orderId = OrderId("🔸")
        primaryMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        primaryMaster.eventWatch.await[OrderFinished](_.key == orderId)
        backupMaster.eventWatch.await[OrderFinished](_.key == orderId)

        primaryMaster.terminate() await 99.s
        backupMaster.terminate() await 99.s
      }
    }
  }
}
