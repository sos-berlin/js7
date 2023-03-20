package js7.tests.controller.cluster

import js7.base.problem.Checked.*
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterSwitchedOver, ClusterWatchRegistered}
import js7.data.cluster.ClusterTiming
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.controller.ControllerCommand.ShutDown.ClusterAction
import monix.execution.Scheduler.Implicits.traced

final class ShutdownSwitchoverActiveControllerClusterTest extends ControllerClusterTester
{
  protected override val clusterTiming = ClusterTiming(heartbeat = 500.ms, heartbeatTimeout = 10.s)

  override protected def removeObsoleteJournalFiles = false

  "ShutDown active node with switchover" in {
    withControllerAndBackup() { (primary, _, backup, _, _) =>
      backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
        primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
          primaryController.eventWatch.await[ClusterWatchRegistered]()
          primaryController.eventWatch.await[ClusterCoupled]()

          primaryController.api
            .executeCommand(ShutDown(clusterAction = Some(ClusterAction.Switchover)))
            .await(99.s).orThrow
          backupController.eventWatch.await[ClusterSwitchedOver]()
          primaryController.terminated.await(99.s)
        }
      }
    }
  }
}
