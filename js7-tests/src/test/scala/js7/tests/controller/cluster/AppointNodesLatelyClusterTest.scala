package js7.tests.controller.cluster

import js7.base.auth.UserId
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.data.Problems.{BackupClusterNodeNotAppointed, ClusterSettingNotUpdatable}
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost, ClusterSettingUpdated, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.Coupled
import js7.data.controller.ControllerCommand.ClusterAppointNodes
import js7.data.event.EventId
import js7.data.order.OrderEvent.{OrderFinished, OrderStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.journal.files.JournalFiles.listJournalFiles
import js7.tests.controller.cluster.ControllerClusterTester.*
import monix.execution.Scheduler.Implicits.traced

final class AppointNodesLatelyClusterTest extends OurTestSuite with ControllerClusterTester
{
  override protected def configureClusterNodes = false

  "ClusterAppointNodes command after first journal file has been deleted, then change Backup's URI" in {
    withControllerAndBackup() { (primary, backup, clusterSetting) =>
      primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
        val orderId = OrderId("🔺")
        primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        primaryController.eventWatch.await[OrderStarted](_.key == orderId)
      }

      primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
        assert(listJournalFiles(primary.controller.dataDir / "state" / "controller").head
          .fileEventId > EventId.BeforeFirst)

        var backupController = backup.newController(httpPort = Some(backupControllerPort))

        backupController.httpApiDefaultLogin(Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD")))
        backupController.httpApi.login() await 99.s
        assert(backupController.httpApi.clusterState.await(99.s) == Left(BackupClusterNodeNotAppointed))

        primaryController.executeCommandForTest(
          ClusterAppointNodes(clusterSetting.idToUri, clusterSetting.activeId)
        ).orThrow
        primaryController.eventWatch.await[ClusterCoupled]()
        primaryController.eventWatch.await[ClusterWatchRegistered]()

        locally {
          val orderId = OrderId("🔸")
          primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
          primaryController.eventWatch.await[OrderFinished](_.key == orderId)
          backupController.eventWatch.await[OrderFinished](_.key == orderId)
        }

        // PREPARE CHANGING BACKUP NODE
        val primaryUri = clusterSetting.idToUri(primaryId)
        val backupUri = clusterSetting.idToUri(backupId)
        assert(!primaryUri.string.exists(_.isUpper))
        assert(!backupUri.string.exists(_.isUpper))
        val updatedBackupSetting = clusterSetting.copy(
          idToUri = clusterSetting.idToUri + (backupId -> Uri(backupUri.string.toUpperCase)))
        assert(updatedBackupSetting != clusterSetting)

        // UPDATING BACKUP URI IS REJECTED WHEN COUPLED
        val clusterAppointNodes = ClusterAppointNodes(
          updatedBackupSetting.idToUri, updatedBackupSetting.activeId)
        assert(primaryController.executeCommandForTest(clusterAppointNodes) == Left(ClusterSettingNotUpdatable))

        // CHANGE BACKUP URI WHEN PASSIVE IS LOST
        locally {
          val eventId = primaryController.eventWatch.lastAddedEventId
          backupController.terminate(dontNotifyActiveNode = true).await(99.s)

          primaryController.eventWatch.await[ClusterPassiveLost](after = eventId)
          primaryController.executeCommandForTest(clusterAppointNodes).orThrow
          primaryController.eventWatch.await[ClusterSettingUpdated](after = eventId)

          backupController = backup.newController(httpPort = Some(backupControllerPort))

          primaryController.eventWatch.await[ClusterCoupled](after = eventId)
          backupController.eventWatch.await[ClusterCoupled](after = eventId)

          assert(primaryController.clusterState.await(99.s).asInstanceOf[Coupled].setting == updatedBackupSetting)
          assert(backupController.clusterState.await(99.s).asInstanceOf[Coupled].setting == updatedBackupSetting)
        }

        primaryController.terminate() await 99.s
        sleep(200.ms) // TODO Early ShutDown seems to be ignored
        backupController.terminate() await 99.s
      }
    }
  }
}
