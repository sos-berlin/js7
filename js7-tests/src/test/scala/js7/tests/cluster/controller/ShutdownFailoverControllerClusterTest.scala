package js7.tests.cluster.controller

import js7.base.problem.Checked.*
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.{Coupled, FailedOver}
import js7.data.cluster.ClusterTiming
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.controller.ControllerCommand.ShutDown.ClusterAction
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced

final class ShutdownFailoverControllerClusterTest extends ControllerClusterTester
{
  protected override val clusterTiming = ClusterTiming(heartbeat = 500.ms, heartbeatTimeout = 5.s)

  override protected def removeObsoleteJournalFiles = false

  "ShutDown active node with failover (for testing)" in {
    withControllerAndBackup() { (primary, _, backup, _, clusterSetting) =>
      backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
        primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
          primaryController.eventWatch.await[ClusterCoupled]()
          primaryController.eventWatch.await[ClusterWatchRegistered]()

          primaryController
            .api.executeCommand(ShutDown(clusterAction = Some(ClusterAction.Failover)))
            .await(99.s).orThrow
          primaryController.terminated.await(99.s)
        }

        backupController.eventWatch.await[ClusterFailedOver]()
        waitForCondition(3.s, 10.ms)(backupController.clusterState.await(99.s).isInstanceOf[FailedOver])
        assert(backupController.clusterState.await(99.s).asInstanceOf[FailedOver].activeId == backupId)

        // When journal file must be truncated due to non-replicated data after failover,
        // the primary Controller wants to start again.
        var restart = true
        while (restart) {
          primary.runController(httpPort = Some(primaryControllerPort), dontWaitUntilReady = true) { primaryController =>
            Task
              .race(
                Task.fromFuture(primaryController.terminated).uncancelable,
                primaryController.eventWatch.awaitAsync[ClusterCoupled](
                  after = primaryController.eventWatch.lastFileEventId))
              .await(99.s)
              .match_ {
                case Left(ProgramTermination(/*restart=*/true)) =>
                case _ =>
                  restart = false
                  // Restarted Primary should have become passive
                  waitForCondition(3.s, 10.ms)(
                    primaryController.clusterState.await(99.s).isInstanceOf[Coupled])
                  assert(primaryController.clusterState.await(99.s) ==
                    backupController.clusterState.await(99.s))
                  assert(primaryController.clusterState.await(99.s) ==
                    Coupled(clusterSetting.copy(activeId = backupId)))

                  backupController.api.executeCommand(ShutDown()).await(99.s).orThrow
                  backupController.terminated await 99.s
              }
          }
        }
      }
    }
  }
}
