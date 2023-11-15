package js7.tests.controller.cluster

import cats.syntax.traverse.*
import com.typesafe.config.ConfigUtil.quoteString
import js7.agent.RunningAgent
import js7.base.auth.UserId
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.CorrelId
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.cluster.ClusterCommon.ClusterWatchAgreedToActivation
import js7.common.pekkohttp.web.data.WebServerPort
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.Problems.{BackupClusterNodeNotAppointed, ClusterSettingNotUpdatable}
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterPassiveLost, ClusterSettingUpdated}
import js7.data.cluster.ClusterSetting
import js7.data.cluster.ClusterState.Coupled
import js7.data.controller.ControllerCommand.{ClusterAppointNodes, ShutDown}
import js7.data.event.EventId
import js7.data.order.OrderEvent.{OrderFinished, OrderStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.journal.files.JournalFiles.listJournalFiles
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.controller.cluster.ControllerClusterTester.*
import monix.execution.Scheduler.Implicits.traced

final class AppointNodesLatelyClusterWithLegacyClusterWatchTest extends AppointNodesLatelyClusterTest {
  override protected val useLegacyServiceClusterWatch = true
}

class AppointNodesLatelyClusterTest extends OurTestSuite with ControllerClusterTester
{
  override protected def configureClusterNodes = false

  "ClusterAppointNodes command after first journal file has been deleted, then change Backup's URI" in {
    withControllerAndBackupWithoutAgents() { (primary, backup, clusterSetting) =>
      val otherClusterWatchPort = findFreeTcpPort()

      if (useLegacyServiceClusterWatch) {
        for (directoryProvider <- Seq(primary, backup)) {
          // Copy password for otherClusterWatch
          (directoryProvider.controller.configDir / "private" / "private.conf") ++=
            "js7.auth.agents." + quoteString(s"http://127.0.0.1:$otherClusterWatchPort") + " = " +
              quoteString(primary.agents(0).password.string) + "\n"
          }
      }

      val agents = primary.startAgents().await(99.s)

      primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
        val orderId = OrderId("🔺")
        primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        primaryController.eventWatch.await[OrderStarted](_.key == orderId)
      }

      primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
        assert(listJournalFiles(primary.controller.dataDir / "state" / "controller").head
          .fileEventId > EventId.BeforeFirst)

        var backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s

        backupController.httpApiDefaultLogin(Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD")))
        backupController.httpApi.login() await 99.s
        assert(backupController.httpApi.clusterState.await(99.s) == Left(BackupClusterNodeNotAppointed))

        primaryController.executeCommandForTest(
          ClusterAppointNodes(clusterSetting.idToUri, clusterSetting.activeId,
            clusterSetting.clusterWatches)
        ).orThrow
        primaryController.eventWatch.await[ClusterCoupled]()
        waitUntilClusterWatchRegistered(primaryController)

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
          updatedBackupSetting.idToUri, updatedBackupSetting.activeId,
          updatedBackupSetting.clusterWatches)
        assert(primaryController.executeCommandForTest(clusterAppointNodes) == Left(ClusterSettingNotUpdatable))

        // CHANGE BACKUP URI WHEN PASSIVE IS LOST
        locally {
          val eventId = primaryController.eventWatch.lastAddedEventId
          backupController.terminate() await 99.s
          primaryController.eventWatch.await[ClusterPassiveLost](after = eventId)
          primaryController.executeCommandForTest(clusterAppointNodes).orThrow
          primaryController.eventWatch.await[ClusterSettingUpdated](after = eventId)

          backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s

          primaryController.eventWatch.await[ClusterCoupled](after = eventId)
          backupController.eventWatch.await[ClusterCoupled](after = eventId)

          assert(primaryController.clusterState.await(99.s).asInstanceOf[Coupled].setting == updatedBackupSetting)
          assert(backupController.clusterState.await(99.s).asInstanceOf[Coupled].setting == updatedBackupSetting)
        }

        var bAgent: RunningAgent = null
        if (useLegacyServiceClusterWatch) {
          // CHANGE CLUSTER WATCH

          // Terminate ClusterWatch
          agents.traverse(_.terminate()).await(99.s)

          // Start new ClusterWatch
          val bAgentUri = Uri(s"http://127.0.0.1:$otherClusterWatchPort")
          assert(updatedBackupSetting.maybeClusterWatchUri != Some(bAgentUri))
          assert(bAgentUri != primary.agents.head.localUri)
          val watchSetting = updatedBackupSetting.copy(
            clusterWatches = updatedBackupSetting.clusterWatches.map(_ => ClusterSetting.Watch(bAgentUri)))
          assert(watchSetting != updatedBackupSetting)

          bAgent = {
            val conf = primary.agentToTree(primary.agentRefs.head.path).agentConfiguration
            RunningAgent.startForTest(conf.copy(webServerPorts = Seq(WebServerPort.localhost(otherClusterWatchPort)))).await(99.s)
          }
          assert(bAgent.localUri == bAgentUri)

          val eventId = primaryController.eventWatch.lastAddedEventId
          CorrelId.bindNewNow(
            primaryController.executeCommandForTest(
              ClusterAppointNodes(watchSetting.idToUri, watchSetting.activeId, watchSetting.clusterWatches)
            ).orThrow)

          primaryController.eventWatch.await[ClusterSettingUpdated](after = eventId)
          backupController.eventWatch.await[ClusterSettingUpdated](after = eventId)

          assert(primaryController.clusterState.await(99.s).isNonEmptyActive(primaryId))
          assert(primaryController.clusterState.await(99.s).asInstanceOf[Coupled].setting == watchSetting)
          assert(backupController.clusterState.await(99.s).asInstanceOf[Coupled].setting == watchSetting)

          val whenClusterWatchAgrees = backupController.testEventBus.when[ClusterWatchAgreedToActivation.type].runToFuture
          primaryController.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ShutDown.ClusterAction.Failover)))
            .await(99.s)
          backupController.eventWatch.await[ClusterFailedOver](after = eventId)

          awaitAndAssert(backupController.clusterState.await(99.s).isNonEmptyActive(backupId))

          whenClusterWatchAgrees await 99.s
        } else {
          primaryController.terminate() await 99.s
        }
        primaryController.terminated await 99.s
        sleep(200.ms) // TODO Early ShutDown seems to be ignored
        backupController.terminate() await 99.s
        Option(bAgent).foreach(_.terminate() await 90.s)
      }
    }
  }
}
