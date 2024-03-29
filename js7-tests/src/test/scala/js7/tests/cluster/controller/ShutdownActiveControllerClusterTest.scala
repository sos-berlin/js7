package js7.tests.cluster.controller

import js7.base.problem.Checked.*
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.Coupled
import js7.data.cluster.{ClusterState, ClusterTiming}
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.event.EventId
import js7.tester.ScalaTestUtils.awaitAndAssert

final class ShutdownActiveControllerClusterTest extends ControllerClusterTester:

  protected override val clusterTiming = ClusterTiming(heartbeat = 500.ms, heartbeatTimeout = 10.s)

  override protected def removeObsoleteJournalFiles = false

  "ShutDown active controller node only (no switchover)" in:
    withControllerAndBackup() { (primary, _, backup, _, clusterSetting) =>
      backup.runController(dontWaitUntilReady = true) { backupController =>
        primary.runController() { primaryController =>
          primaryController.eventWatch.await[ClusterWatchRegistered]()
          primaryController.eventWatch.await[ClusterCoupled]()
          assert(primaryController.clusterState.await(99.s) == Coupled(clusterSetting))

          primaryController.api.executeCommand(ShutDown())
            .await(99.s).orThrow
          primaryController.terminated.await(99.s)
        }

        val activeNodeShutDown = backupController.eventWatch.await[ClusterActiveNodeShutDown](after = EventId.BeforeFirst).head.eventId

        assert(backupController.clusterState.await(99.s) == ClusterState.ActiveShutDown(clusterSetting))

        primary.runController() { primaryController =>
          val activeRestarted = primaryController.eventWatch.await[ClusterActiveNodeRestarted](
            after = activeNodeShutDown).head.eventId
          primaryController.eventWatch.await[ClusterCoupled](after = activeRestarted)
          // TODO EventWatch delivers event after it has been written but before ack!
          awaitAndAssert(2.s)(primaryController.clusterState.await(99.s) == Coupled(clusterSetting))
        }
      }
    }
