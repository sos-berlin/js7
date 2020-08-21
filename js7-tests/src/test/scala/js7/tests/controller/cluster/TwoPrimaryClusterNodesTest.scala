package js7.tests.controller.cluster

import js7.base.problem.Checked._
import js7.base.problem.ProblemException
import js7.base.time.ScalaTime._
import js7.common.configutils.Configs._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.controller.data.ControllerCommand.ClusterAppointNodes
import js7.core.problems.PrimaryMayNotBecomeBackupProblem
import js7.data.node.NodeId
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class TwoPrimaryClusterNodesTest extends AnyFreeSpec with ControllerClusterTester
{
  override protected def configureClusterNodes = false

  "ClusterAppointNodes is rejected if backup cluster node is not configured as a backup" in {
    withControllerAndBackup() { (primary, backup) =>
      primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
        backup.runController(
          httpPort = Some(backupControllerPort),
          config = config"js7.journal.cluster.node.is-backup = false"
        ) { backupController =>
          val cmd = ClusterAppointNodes(
            Map(
              NodeId("Primary") -> primaryController.localUri,
              NodeId("Backup") -> backupController.localUri),
            NodeId("Primary"))
          primaryController.executeCommandAsSystemUser(cmd).await(99.s).orThrow
          sleep(5.s)
          //assert(primaryController.executeCommandAsSystemUser(cmd).await(99.s) == Left(ClusterNodeIsNotBackupProblem))
        }
      }
    }
  }

  "An active primary may not be configured as a backup node" in {
    withControllerAndBackup() { (primary, _) =>
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
      assert(t.problem == PrimaryMayNotBecomeBackupProblem)
    }
  }
}
