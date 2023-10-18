package js7.tests.cluster.controller

import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.data.Problems.BackupClusterNodeNotAppointed
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost, ClusterSettingUpdated, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.Coupled
import js7.data.controller.ControllerCommand.ClusterAppointNodes
import js7.data.event.EventId
import js7.data.order.OrderEvent.{OrderFinished, OrderStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.journal.files.JournalFiles.listJournalFiles
import js7.tests.cluster.controller.ControllerClusterTester.*
import monix.execution.Scheduler.Implicits.traced

final class AppointNodesLatelyControllerClusterTest extends OurTestSuite, ControllerClusterTester:

  override protected def configureClusterNodes = false

  "ClusterAppointNodes command after first journal file has been deleted, then change Backup's URI" in:
    withControllerAndBackup() { (primary, _, backup, _, clusterSetting) =>
      primary.runController() { primaryController =>
        val orderId = OrderId("🔺")
        primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        primaryController.eventWatch.await[OrderStarted](_.key == orderId)
      }

      primary.runController() { primaryController =>
        assert(listJournalFiles(primary.controllerEnv.dataDir / "state" / "controller").head
          .fileEventId > EventId.BeforeFirst)

        var backupController = backup.newController()

        assert(backupController.api.clusterState.await(99.s) == Left(BackupClusterNodeNotAppointed))

        primaryController.api.executeCommand(
          ClusterAppointNodes(clusterSetting.idToUri, clusterSetting.activeId)
        ).await(99.s).orThrow
        primaryController.eventWatch.await[ClusterCoupled]()
        primaryController.eventWatch.await[ClusterWatchRegistered]()

        locally:
          val orderId = OrderId("🔸")
          primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
          primaryController.eventWatch.await[OrderFinished](_.key == orderId)
          backupController.eventWatch.await[OrderFinished](_.key == orderId)

        // PREPARE CHANGING BACKUP NODE
        val primaryUri = clusterSetting.idToUri(primaryId)
        val backupUri = clusterSetting.idToUri(backupId)
        assert(!primaryUri.string.exists(_.isUpper))
        assert(!backupUri.string.exists(_.isUpper))
        val updatedBackupSetting = clusterSetting.copy(
          idToUri = clusterSetting.idToUri + (backupId -> Uri(backupUri.string.toUpperCase)))
        assert(updatedBackupSetting != clusterSetting)

        val clusterAppointNodes = ClusterAppointNodes(
          updatedBackupSetting.idToUri, updatedBackupSetting.activeId)
        // UPDATING BACKUP URI IS REJECTED WHEN COUPLED
        //assert(primaryController.api.executeCommand(clusterAppointNodes).await(99.s)
        //  .left.exists(_ is ClusterSettingNotUpdatable))

        // CHANGE BACKUP URI WHEN PASSIVE IS LOST
        locally:
          val eventId = primaryController.eventWatch.lastAddedEventId
          backupController.terminate(dontNotifyActiveNode = true).await(99.s)

          primaryController.eventWatch.await[ClusterPassiveLost](after = eventId)
          primaryController.api.executeCommand(clusterAppointNodes).await(99.s).orThrow
          primaryController.eventWatch.await[ClusterSettingUpdated](after = eventId)

          backupController = backup.newController()

          primaryController.eventWatch.await[ClusterCoupled](after = eventId)
          backupController.eventWatch.await[ClusterCoupled](after = eventId)

          assert(primaryController.clusterState.await(99.s).asInstanceOf[Coupled].setting == updatedBackupSetting)
          assert(backupController.clusterState.await(99.s).asInstanceOf[Coupled].setting == updatedBackupSetting)

        primaryController.terminate() await 99.s
        sleep(200.ms) // TODO Early ShutDown seems to be ignored
        backupController.terminate() await 99.s
      }
    }
