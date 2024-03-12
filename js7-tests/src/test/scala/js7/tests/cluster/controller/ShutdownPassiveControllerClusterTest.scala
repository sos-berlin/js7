package js7.tests.cluster.controller

import js7.base.problem.Checked.*
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.Problems.PassiveClusterNodeShutdownNotAllowedProblem
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost, ClusterWatchRegistered}
import js7.data.cluster.ClusterTiming
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.controller.ControllerCommand.ShutDown.ClusterAction

final class ShutdownPassiveControllerClusterTest extends ControllerClusterTester:

  protected override val clusterTiming = ClusterTiming(heartbeat = 500.ms, heartbeatTimeout = 5.s)

  override protected def removeObsoleteJournalFiles = false

  "ShutDown passive node only (no switchover)" in:
    withControllerAndBackup() { (primary, _, backup, _, _) =>
      val backupController = backup.newController()
      primary.runController() { primaryController =>
        primaryController.eventWatch.await[ClusterWatchRegistered]()
        primaryController.eventWatch.await[ClusterCoupled]()

        backupController.api.executeCommand(ShutDown()).await(99.s).orThrow
        backupController.stop.await(99.s)

        primaryController.eventWatch.await[ClusterPassiveLost]()
      }
    }

  "ShutDown passive node with switchover or failover is rejected" in:
    withControllerAndBackup() { (primary, _, backup, _, _) =>
      backup.runController(dontWaitUntilReady = true) { backupController =>
        primary.runController() { primaryController =>
          //waitUntilClusterWatchRegistered(primaryController)
          primaryController.eventWatch.await[ClusterCoupled]()

          backupController.api.executeCommand(ShutDown(clusterAction = Some(ClusterAction.Switchover)))
            .await(99.s).left.map(_ is PassiveClusterNodeShutdownNotAllowedProblem)
          backupController.api.executeCommand(ShutDown(clusterAction = Some(ClusterAction.Failover)))
            .await(99.s).left.map(_ is PassiveClusterNodeShutdownNotAllowedProblem)
        }
      }
    }
