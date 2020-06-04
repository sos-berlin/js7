package com.sos.jobscheduler.tests.master.cluster

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.listJournalFiles
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterNodeId}
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.order.OrderEvent.{OrderFinished, OrderStarted}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.master.data.MasterCommand.ClusterAppointNodes
import com.sos.jobscheduler.tests.master.cluster.MasterClusterTester._
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
