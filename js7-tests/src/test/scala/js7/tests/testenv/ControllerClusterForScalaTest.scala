package js7.tests.testenv

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Files
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.utils.CatsUtils.combineArgs
import js7.base.utils.Closer.syntax._
import js7.base.utils.Closer.withCloser
import js7.common.auth.SecretStringGenerator
import js7.common.configutils.Configs._
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.message.ProblemCodeMessages
import js7.common.scalautil.FileUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.RunningController
import js7.controller.data.ControllerCommand.ShutDown
import js7.core.event.journal.files.JournalFiles.listJournalFiles
import js7.data.agent.AgentRefPath
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.item.InventoryItem
import js7.data.job.ExecutablePath
import js7.data.node.NodeId
import js7.tests.testenv.DirectoryProvider.script
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalactic.source
import org.scalatest.Assertions._
import org.scalatest.freespec.AnyFreeSpec

trait ControllerClusterForScalaTest extends AnyFreeSpec
{
  protected def agentRefPaths: Seq[AgentRefPath] = AgentRefPath("/AGENT") :: Nil
  protected def inventoryItems: Seq[InventoryItem]
  protected def shellScript = script(0.s)

  protected def configureClusterNodes = true
  protected def removeObsoleteJournalFiles = true
  protected final val primaryId = NodeId("Primary")
  protected final val backupId = NodeId("Backup")

  protected def primaryControllerConfig: Config = ConfigFactory.empty
  protected def backupControllerConfig: Config = ConfigFactory.empty

  coupleScribeWithSlf4j()
  ProblemCodeMessages.initialize()
  protected final val testHeartbeatLossPropertyKey = "js7.TEST." + SecretStringGenerator.randomString()
  sys.props(testHeartbeatLossPropertyKey) = "false"

  final def runControllerAndBackup(primaryHttpPort: Int = findFreeTcpPort(), backupHttpPort: Int = findFreeTcpPort())
    (body: (DirectoryProvider, RunningController, DirectoryProvider, RunningController) => Unit)
  : Unit =
    withControllerAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      backup.runController(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupController =>
        primary.runController(httpPort = Some(primaryHttpPort)) { primaryController =>
          primaryController.eventWatch.await[ClusterCoupled]()
          body(primary, primaryController, backup, backupController)
        }
      }
    }

  final def withControllerAndBackup(primaryHttpPort: Int = findFreeTcpPort(), backupHttpPort: Int = findFreeTcpPort())
    (body: (DirectoryProvider, DirectoryProvider) => Unit)
  : Unit =
    withCloser { implicit closer =>
      val testName = ControllerClusterForScalaTest.this.getClass.getSimpleName
      val agentPort = findFreeTcpPort()
      val primary = new DirectoryProvider(agentRefPaths, inventoryItems, testName = Some(s"$testName-Primary"),
        controllerConfig = combineArgs(
          primaryControllerConfig,
          configIf(configureClusterNodes, config"""
            js7.journal.cluster.nodes = {
              Primary: "http://127.0.0.1:$primaryHttpPort"
              Backup: "http://127.0.0.1:$backupHttpPort"
            }"""),
          config"""
            js7.journal.cluster.heartbeat = 0.5s
            js7.journal.cluster.fail-after = 5s
            js7.journal.cluster.watches = [ "http://127.0.0.1:$agentPort" ]
            js7.journal.cluster.TEST-HEARTBEAT-LOSS = "$testHeartbeatLossPropertyKey"
            js7.journal.use-journaled-state-as-snapshot = true
            js7.journal.release-events-delay = 0s
            js7.journal.remove-obsolete-files = $removeObsoleteJournalFiles
            js7.auth.users.TEST-USER.password = "plain:TEST-PASSWORD"
            js7.auth.users.Controller.password = "plain:PRIMARY-CONTROLLER-PASSWORD"
            js7.auth.cluster.password = "BACKUP-CONTROLLER-PASSWORD" """),
        agentPorts = agentPort :: Nil
      ).closeWithCloser

      val backup = new DirectoryProvider(Nil, Nil, testName = Some(s"$testName-Backup"),
        controllerConfig = combineArgs(
          backupControllerConfig,
          config"""
            js7.journal.cluster.node.is-backup = yes
            js7.journal.cluster.heartbeat = 3s
            js7.journal.cluster.fail-after = 5s
            js7.journal.cluster.watches = [ "http://127.0.0.1:$agentPort" ]
            js7.journal.cluster.TEST-HEARTBEAT-LOSS = "$testHeartbeatLossPropertyKey"
            js7.journal.use-journaled-state-as-snapshot = true
            js7.journal.release-events-delay = 0s
            js7.journal.remove-obsolete-files = $removeObsoleteJournalFiles
            js7.auth.users.Controller.password = "plain:BACKUP-CONTROLLER-PASSWORD"
            js7.auth.users.TEST-USER.password = "plain:TEST-PASSWORD"
            js7.auth.cluster.password = "PRIMARY-CONTROLLER-PASSWORD""""),
      ).closeWithCloser

      // Replicate credentials required for agents
      Files.copy(
        primary.controller.configDir / "private" / "private.conf",
        backup.controller.configDir / "private" / "private.conf",
        REPLACE_EXISTING)

      for (a <- primary.agents) a.writeExecutable(ExecutablePath("/TEST.cmd"), shellScript)

      primary.runAgents() { _ =>
        body(primary, backup)
      }
    }

  /** Simulate a kill via ShutDown(failOver) - still writes new snapshot. */
  protected final def simulateKillActiveNode(controller: RunningController)(implicit s: Scheduler): Task[Unit] =
    controller.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ShutDown.ClusterAction.Failover)))
      .map(_.orThrow)
      .flatMap(_ => Task.deferFuture(controller.terminated))
      .map(_ => ())
}

object ControllerClusterForScalaTest
{
  def assertEqualJournalFiles(
    primary: DirectoryProvider.ControllerTree,
    backup: DirectoryProvider.ControllerTree,
    n: Int)
    (implicit pos: source.Position)
  : Unit = {
    waitForCondition(9.s, 10.ms) { listJournalFiles(primary.stateDir / "controller").size == n }
    val journalFiles = listJournalFiles(primary.stateDir / "controller")
    // Snapshot is not being acknowledged, so a new journal file starts asynchronously (or when one event has been written)
    assert(journalFiles.size == n)
    waitForCondition(9.s, 10.ms) { listJournalFiles(backup.stateDir / "controller").size == n }
    for (primaryFile <- journalFiles.map(_.file)) {
      withClue(s"$primaryFile: ") {
        val backupJournalFile = backup.stateDir.resolve(primaryFile.getFileName)
        assert(backupJournalFile.contentString == primaryFile.contentString)
        assert(backupJournalFile.byteVector == primaryFile.byteVector)
      }
    }
  }
}
