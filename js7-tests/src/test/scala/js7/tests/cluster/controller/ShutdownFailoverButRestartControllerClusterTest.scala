package js7.tests.cluster.controller

import js7.base.problem.Checked.*
import js7.base.thread.Futures.implicits.*
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.cluster.ClusterState.Coupled
import js7.data.cluster.ClusterTiming
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.controller.ControllerCommand.ShutDown.ClusterAction

final class ShutdownFailoverButRestartControllerClusterTest extends ControllerClusterTester:
  protected override val clusterTiming = ClusterTiming(heartbeat = 500.ms, heartbeatTimeout = 5.s)

  override protected def removeObsoleteJournalFiles = false

  "ShutDown active node with failover requested (for testing), then immediate restart of the shut down node" in:
    withControllerAndBackup() { (primary, _, backup, _, clusterSetting) =>
      backup.runController(dontWaitUntilReady = true) { backupController =>
        var eventId = 0L
        primary.runController() { primaryController =>
          primaryController.eventWatch.await[ClusterCoupled]()

          eventId = primaryController.eventWatch.lastAddedEventId
          primaryController.api.executeCommand(ShutDown(clusterAction = Some(ClusterAction.Failover)))
            .await(99.s).orThrow
          primaryController.terminated.await(99.s)
        }
        primary.runController() { primaryController =>
          primaryController.eventWatch.await[ClusterCoupled](after = eventId)
          backupController.eventWatch.await[ClusterCoupled](after = eventId)
          assert(primaryController.clusterState.await(99.s) == Coupled(clusterSetting))
          assert(backupController.clusterState.await(99.s) == Coupled(clusterSetting))

          // TODO When Primary Controller is shutting down before started (no ControllerOrderKeeper)
          //  while the Backup Controller is shutting down and times out the acknowledgement request,
          //  Cluster asks the JournalActor about ClusterState for issuing a ClusterPassiveLost event.
          //  But JournalActor does not answer if not started.
          primaryController.waitUntilReady()

          backupController.api.executeCommand(ShutDown()).await(99.s).orThrow
          backupController.terminated await 99.s
        }
      }
    }
