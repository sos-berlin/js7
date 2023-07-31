package js7.tests.cluster.controller

import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterWatchRegistered}
import js7.data.event.KeyedEvent.NoKey
import monix.execution.Scheduler.Implicits.traced

final class ActiveWebServerIsUnreachableClusterTest extends ControllerClusterTester
{
  "Only the active node's web server is unreachable" in {
    sys.props(testHeartbeatLossPropertyKey) = "false"
    withControllerAndBackup() { (primary, _, backup, _, _) =>
      val primaryController = primary.newController()
      val backupController = backup.newController()

      primaryController.eventWatch.await[ClusterWatchRegistered]()
      primaryController.eventWatch.await[ClusterCoupled]()

      primaryController.runningController.webServer.stopSingleWebServers.await(99.s)

      backupController.eventWatch.await[ClusterFailedOver](_.key == NoKey)

      // Primary must have terminate itself because it gets AckFromActiveClusterNodeProblem
      // (when not isTest, primary should halt)
      primaryController.terminated.await(99.s)
      backupController.stop.await(99.s)
    }
  }
}
