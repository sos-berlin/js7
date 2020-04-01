package com.sos.jobscheduler.tests.master.cluster

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Closer.syntax._
import com.sos.jobscheduler.base.utils.Closer.withCloser
import com.sos.jobscheduler.base.utils.Strings._
import com.sos.jobscheduler.common.auth.SecretStringGenerator
import com.sos.jobscheduler.common.log.ScribeUtils
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPort
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.listJournalFiles
import com.sos.jobscheduler.core.message.ProblemCodeMessages
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.tests.master.cluster.MasterClusterTester.{shellScript, _}
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import com.typesafe.config.ConfigFactory
import java.nio.file.Files
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import org.scalatest.Assertions._
import org.scalatest.FreeSpec

private[cluster] trait MasterClusterTester extends FreeSpec
{
  protected def configureClusterNodes = true

  ScribeUtils.coupleScribeWithSlf4j()
  ProblemCodeMessages.initialize()
  protected final val testHeartbeatLossPropertyKey = "jobscheduler.TEST." + SecretStringGenerator.randomString()
  sys.props(testHeartbeatLossPropertyKey) = "false"

  protected[cluster] final def withMasterAndBackup(primaryHttpPort: Int, backupHttpPort: Int)(body: (DirectoryProvider, DirectoryProvider) => Unit): Unit =
    withCloser { implicit closer =>
      val testName = MasterClusterTester.this.getClass.getSimpleName
      val agentPort = findFreeTcpPort()
      val primary = new DirectoryProvider(agentRefPath :: Nil, TestWorkflow :: Nil, testName = Some(s"$testName-Primary"),
        masterConfig = ConfigFactory.parseString((configureClusterNodes ?: s"""
          jobscheduler.master.cluster.nodes = {
            Primary: "http://127.0.0.1:$primaryHttpPort"
            Backup: "http://127.0.0.1:$backupHttpPort"
          }""") + s"""
          jobscheduler.master.cluster.heartbeat = 3s
          jobscheduler.master.cluster.fail-after = 5s
          jobscheduler.master.cluster.watches = [ "http://127.0.0.1:$agentPort" ]
          jobscheduler.master.cluster.TEST-HEARTBEAT-LOSS = "$testHeartbeatLossPropertyKey"
          jobscheduler.auth.users.Master.password = "plain:BACKUP-MASTER-PASSWORD"
          jobscheduler.auth.users.TEST.password = "plain:TEST-PASSWORD"
          jobscheduler.auth.cluster.password = "PRIMARY-MASTER-PASSWORD" """),
        agentPorts = agentPort :: Nil
      ).closeWithCloser

      val backup = new DirectoryProvider(Nil, Nil, testName = Some(s"$testName-Backup"),
        masterConfig = ConfigFactory.parseString(s"""
          jobscheduler.master.cluster.node.is-backup = yes
          jobscheduler.master.cluster.heartbeat = 3s
          jobscheduler.master.cluster.fail-after = 5s
          jobscheduler.master.cluster.watches = [ "http://127.0.0.1:$agentPort" ]
          jobscheduler.master.cluster.TEST-HEARTBEAT-LOSS = "$testHeartbeatLossPropertyKey"
          jobscheduler.auth.users.Master.password = "plain:PRIMARY-MASTER-PASSWORD"
          jobscheduler.auth.cluster.password = "BACKUP-MASTER-PASSWORD" """)
      ).closeWithCloser

      // Replicate credentials required for agents
      Files.copy(
        primary.master.configDir / "private" / "private.conf",
        backup.master.configDir / "private" / "private.conf",
        REPLACE_EXISTING)

      primary.agents.head.writeExecutable(ExecutablePath("/TEST.cmd"), shellScript)

      primary.runAgents() { _ =>
        body(primary, backup)
      }
    }
}

object MasterClusterTester
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

  private[cluster] def assertEqualJournalFiles(primary: DirectoryProvider.MasterTree, backup: DirectoryProvider.MasterTree, n: Int): Unit = {
    val journalFiles = listJournalFiles(primary.stateDir / "master")
    // Snapshot is not being acknowledged, so a new journal file starts asynchronously (or when one event has been written)
    assert(journalFiles.size == n)
    waitForCondition(9.s, 10.ms) { listJournalFiles(backup.stateDir / "master").size == n }
    for (primaryFile <- journalFiles.map(_.file)) {
      withClue(s"$primaryFile: ") {
        val backupJournalFile = backup.stateDir.resolve(primaryFile.getFileName)
        assert(backupJournalFile.contentString == primaryFile.contentString)
        assert(backupJournalFile.byteVector == primaryFile.byteVector)
      }
    }
  }
}
