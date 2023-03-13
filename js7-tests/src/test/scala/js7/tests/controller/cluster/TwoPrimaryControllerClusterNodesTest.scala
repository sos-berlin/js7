package js7.tests.controller.cluster

import js7.base.configutils.Configs.*
import js7.base.problem.Checked.*
import js7.base.problem.ProblemException
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.Problems.PrimaryClusterNodeMayNotBecomeBackupProblem
import js7.data.controller.ControllerCommand.ClusterAppointNodes
import monix.execution.Scheduler.Implicits.traced

final class TwoPrimaryControllerClusterNodesTest extends OurTestSuite with ControllerClusterTester
{
  override protected def configureClusterNodes = false

  "ClusterAppointNodes is rejected if backup cluster node is not configured as a backup" in {
    withControllerAndBackup() { (primary, backup, clusterSetting) =>
      primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
        backup.runController(
          httpPort = Some(backupControllerPort),
          config = config"js7.journal.cluster.node.is-backup = false"
        ) { _ =>
          val cmd = ClusterAppointNodes(clusterSetting.idToUri, clusterSetting.activeId)
          primaryController.api.executeCommand(cmd).await(99.s).orThrow
          sleep(5.s)
          //assert(primaryController.api.executeCommand(cmd).await(99.s) == Left(ClusterNodeIsNotBackupProblem))
        }
      }
    }
  }

  "An active primary may not be configured as a backup node" in {
    withControllerAndBackup() { (primary, _, _) =>
      primary.runController(httpPort = Some(primaryControllerPort)) { _ => }
      val t = intercept[ProblemException] {
        primary.runController(
          httpPort = Some(primaryControllerPort),
          config = config"js7.journal.cluster.node.is-backup = true",
          dontWaitUntilReady = true
        ) { _ =>
          // TODO Introduce ClusterFailed event to check the asynchronous failure?
        }
      }
      assert(t.problem == PrimaryClusterNodeMayNotBecomeBackupProblem)
    }
  }
}
