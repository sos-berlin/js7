package js7.tests.controller.cluster

import com.typesafe.config.ConfigFactory
import js7.base.problem.Checked._
import js7.base.problem.ProblemException
import js7.base.time.ScalaTime._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.controller.data.ControllerCommand.ClusterAppointNodes
import js7.core.problems.PrimaryMayNotBecomeBackupProblem
import js7.data.node.NodeId
import monix.execution.Scheduler.Implicits.global
final class TwoPrimaryClusterNodesTest extends ControllerClusterTester
{
  override protected def configureClusterNodes = false

  "ClusterAppointNodes is rejected if backup cluster node is not configured as a backup" in {
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withControllerAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      primary.runController(httpPort = Some(primaryHttpPort)) { primaryController =>
        backup.runController(
          httpPort = Some(backupHttpPort),
          config = ConfigFactory.parseString("js7.journal.cluster.node.is-backup = false")
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
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withControllerAndBackup(primaryHttpPort, backupHttpPort) { (primary, _) =>
      primary.runController(httpPort = Some(primaryHttpPort)) { _ => }
      val t = intercept[ProblemException] {
        primary.runController(
          httpPort = Some(primaryHttpPort),
          config = ConfigFactory.parseString("js7.journal.cluster.node.is-backup = true"),
          dontWaitUntilReady = true
        ) { _ =>
          // TODO Introduce ClusterFailed event to check the asynchronous failure?
        }
      }
      assert(t.problem == PrimaryMayNotBecomeBackupProblem)
    }
  }
}
