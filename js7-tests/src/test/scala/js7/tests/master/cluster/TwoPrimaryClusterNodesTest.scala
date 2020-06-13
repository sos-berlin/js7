package js7.tests.master.cluster

import com.typesafe.config.ConfigFactory
import js7.base.problem.Checked._
import js7.base.problem.ProblemException
import js7.base.time.ScalaTime._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.core.problems.PrimaryMayNotBecomeBackupProblem
import js7.data.cluster.ClusterNodeId
import js7.master.data.MasterCommand.ClusterAppointNodes
import monix.execution.Scheduler.Implicits.global
final class TwoPrimaryClusterNodesTest extends MasterClusterTester
{
  override protected def configureClusterNodes = false

  "ClusterAppointNodes is rejected if backup cluster node is not configured as a backup" in {
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
        backup.runMaster(
          httpPort = Some(backupHttpPort),
          config = ConfigFactory.parseString("js7.master.cluster.node.is-backup = false")
        ) { backupMaster =>
          val cmd = ClusterAppointNodes(
            Map(
              ClusterNodeId("Primary") -> primaryMaster.localUri,
              ClusterNodeId("Backup") -> backupMaster.localUri),
            ClusterNodeId("Primary"))
          primaryMaster.executeCommandAsSystemUser(cmd).await(99.s).orThrow
          sleep(5.s)
          //assert(primaryMaster.executeCommandAsSystemUser(cmd).await(99.s) == Left(ClusterNodeIsNotBackupProblem))
        }
      }
    }
  }

  "An active primary may not be configured as a backup node" in {
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, _) =>
      primary.runMaster(httpPort = Some(primaryHttpPort)) { _ => }
      val t = intercept[ProblemException] {
        primary.runMaster(
          httpPort = Some(primaryHttpPort),
          config = ConfigFactory.parseString("js7.master.cluster.node.is-backup = true"),
          dontWaitUntilReady = true
        ) { _ =>
          // TODO Introduce ClusterFailed event to check the asynchronous failure?
        }
      }
      assert(t.problem == PrimaryMayNotBecomeBackupProblem)
    }
  }
}
