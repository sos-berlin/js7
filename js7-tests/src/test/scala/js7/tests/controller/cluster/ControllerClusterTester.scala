package js7.tests.controller.cluster

import com.typesafe.config.ConfigFactory
import java.nio.file.Files
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax._
import js7.base.utils.Closer.withCloser
import js7.base.utils.ScalaUtils.syntax._
import js7.common.auth.SecretStringGenerator
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.scalautil.FileUtils.syntax._
import js7.common.system.OperatingSystem.isWindows
import js7.common.time.WaitForCondition.waitForCondition
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.RunningController
import js7.controller.data.ControllerCommand.ShutDown
import js7.core.event.journal.files.JournalFiles.listJournalFiles
import js7.core.message.ProblemCodeMessages
import js7.data.agent.AgentRefPath
import js7.data.cluster.ClusterNodeId
import js7.data.job.ExecutablePath
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.tests.controller.cluster.ControllerClusterTester._
import js7.tests.testenv.DirectoryProvider
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.Assertions._
import org.scalatest.freespec.AnyFreeSpec

private[cluster] trait ControllerClusterTester extends AnyFreeSpec
{
  protected def configureClusterNodes = true
  protected def removeObsoleteJournalFiles = true
  protected final val primaryId = ClusterNodeId("Primary")
  protected final val backupId = ClusterNodeId("Backup")

  coupleScribeWithSlf4j()
  ProblemCodeMessages.initialize()
  protected final val testHeartbeatLossPropertyKey = "js7.TEST." + SecretStringGenerator.randomString()
  sys.props(testHeartbeatLossPropertyKey) = "false"

  protected[cluster] final def withControllerAndBackup(primaryHttpPort: Int, backupHttpPort: Int)
    (body: (DirectoryProvider, DirectoryProvider) => Unit)
  : Unit =
    withCloser { implicit closer =>
      val testName = ControllerClusterTester.this.getClass.getSimpleName
      val agentPort = findFreeTcpPort()
      val primary = new DirectoryProvider(agentRefPath :: Nil, TestWorkflow :: Nil, testName = Some(s"$testName-Primary"),
        controllerConfig = ConfigFactory.parseString((configureClusterNodes ?: s"""
          js7.journal.cluster.nodes = {
            Primary: "http://127.0.0.1:$primaryHttpPort"
            Backup: "http://127.0.0.1:$backupHttpPort"
          }""") + s"""
          js7.journal.cluster.heartbeat = 3s
          js7.journal.cluster.fail-after = 5s
          js7.journal.cluster.watches = [ "http://127.0.0.1:$agentPort" ]
          js7.journal.cluster.TEST-HEARTBEAT-LOSS = "$testHeartbeatLossPropertyKey"
          js7.journal.use-journaled-state-as-snapshot = true
          js7.journal.remove-obsolete-files = $removeObsoleteJournalFiles
          js7.auth.users.TEST.password = "plain:TEST-PASSWORD"
          js7.auth.users.Controller.password = "plain:PRIMARY-CONTROLLER-PASSWORD"
          js7.auth.cluster.password = "BACKUP-CONTROLLER-PASSWORD""""),
        agentPorts = agentPort :: Nil
      ).closeWithCloser

      val backup = new DirectoryProvider(Nil, Nil, testName = Some(s"$testName-Backup"),
        controllerConfig = ConfigFactory.parseString(s"""
          js7.journal.cluster.node.is-backup = yes
          js7.journal.cluster.heartbeat = 3s
          js7.journal.cluster.fail-after = 5s
          js7.journal.cluster.watches = [ "http://127.0.0.1:$agentPort" ]
          js7.journal.cluster.TEST-HEARTBEAT-LOSS = "$testHeartbeatLossPropertyKey"
          js7.journal.use-journaled-state-as-snapshot = true
          js7.journal.remove-obsolete-files = $removeObsoleteJournalFiles
          js7.auth.users.Controller.password = "plain:BACKUP-CONTROLLER-PASSWORD"
          js7.auth.users.TEST.password = "plain:TEST-PASSWORD"
          js7.auth.cluster.password = "PRIMARY-CONTROLLER-PASSWORD""""),
      ).closeWithCloser

      // Replicate credentials required for agents
      Files.copy(
        primary.controller.configDir / "private" / "private.conf",
        backup.controller.configDir / "private" / "private.conf",
        REPLACE_EXISTING)

      primary.agents.head.writeExecutable(ExecutablePath("/TEST.cmd"), shellScript)

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

object ControllerClusterTester
{
  private val agentRefPath = AgentRefPath("/AGENT")
  private[cluster] val TestWorkflow = WorkflowParser.parse(
    WorkflowPath("/WORKFLOW"),
    """define workflow {
      |  execute executable="/TEST.cmd", agent="/AGENT";
      |}""".stripMargin).orThrow

  private val shellScript = {
    val stdoutSize = 1*1000*1000
    val line = "." * 999
    (if (isWindows)
      """@echo off
        |if not "%SCHEDULER_PARAM_SLEEP" == "" ping -n 2 127.0.0.1 >nul
        |if not "%SCHEDULER_PARAM_SLEEP" == "" ping -n %SCHEDULER_PARAM_SLEEP 127.0.0.1 >nul
        |""".stripMargin
     else
      """[ -z "$SCHEDULER_PARAM_SLEEP" ] || sleep $SCHEDULER_PARAM_SLEEP
        |""".stripMargin
    ) +
      (1 to stdoutSize / line.length)
        .map(i => "echo " + s"$i $line".take(line.length) + "\n")
        .mkString
  }

  private[cluster] def assertEqualJournalFiles(primary: DirectoryProvider.ControllerTree, backup: DirectoryProvider.ControllerTree, n: Int): Unit = {
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
