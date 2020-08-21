package js7.tests.controller.cluster

import js7.base.auth.UserId
import js7.base.generic.SecretString
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.controller.data.ControllerCommand.ClusterAppointNodes
import js7.core.event.journal.files.JournalFiles.listJournalFiles
import js7.core.problems.BackupClusterNodeNotAppointed
import js7.data.cluster.ClusterEvent
import js7.data.event.EventId
import js7.data.node.NodeId
import js7.data.order.OrderEvent.{OrderFinished, OrderStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.tests.controller.cluster.ControllerClusterTester._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class AppointNodesLatelyClusterTest extends AnyFreeSpec with ControllerClusterTester
{
  override protected def configureClusterNodes = false

  "ClusterAppointNodes command after first journal file has been deleted" in {
    withControllerAndBackup() { (primary, backup) =>
      primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
        val orderId = OrderId("🔺")
        primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        primaryController.eventWatch.await[OrderStarted](_.key == orderId)
      }

      primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
        assert(listJournalFiles(primary.controller.dataDir / "state" / "controller").head.afterEventId > EventId.BeforeFirst)

        val backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s

        backupController.httpApiDefaultLogin(Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD")))
        backupController.httpApi.login() await 99.s
        assert(backupController.httpApi.clusterState.await(99.s) == Left(BackupClusterNodeNotAppointed))

        primaryController.executeCommandAsSystemUser(
          ClusterAppointNodes(
            Map(
              NodeId("Primary") -> primaryController.localUri,
              NodeId("Backup") -> backupController.localUri),
            NodeId("Primary"))
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
