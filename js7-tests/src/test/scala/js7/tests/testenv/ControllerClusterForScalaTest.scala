package js7.tests.testenv

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Files
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.configutils.Configs._
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax._
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.CatsUtils.combine
import js7.base.utils.Closer.syntax._
import js7.base.utils.Closer.withCloser
import js7.base.utils.ProgramTermination
import js7.base.web.Uri
import js7.common.auth.SecretStringGenerator
import js7.common.message.ProblemCodeMessages
import js7.common.utils.FreeTcpPortFinder.{findFreeTcpPort, findFreeTcpPorts}
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.cluster.{ClusterSetting, ClusterTiming}
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.item.InventoryItem
import js7.data.job.RelativePathExecutable
import js7.data.node.NodeId
import js7.journal.files.JournalFiles.listJournalFiles
import js7.tests.testenv.ControllerClusterForScalaTest.TestPathExecutable
import js7.tests.testenv.DirectoryProvider.script
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalactic.source
import org.scalatest.Assertions._
import org.scalatest.TestSuite

trait ControllerClusterForScalaTest
{
  this: TestSuite =>

  protected def agentPaths: Seq[AgentPath] = AgentPath("AGENT") :: Nil
  protected def items: Seq[InventoryItem]
  protected def shellScript = script(0.s)

  protected def configureClusterNodes = true
  protected def removeObsoleteJournalFiles = true
  protected final val primaryId = NodeId("Primary")
  protected final val backupId = NodeId("Backup")

  protected def primaryControllerConfig: Config = ConfigFactory.empty
  protected def backupControllerConfig: Config = ConfigFactory.empty

  protected lazy val primaryControllerPort = findFreeTcpPort()
  protected lazy val backupControllerPort = findFreeTcpPort()

  protected val clusterTiming = ClusterTiming(1.s, 3.s)

  coupleScribeWithSlf4j()
  ProblemCodeMessages.initialize()
  protected final val testHeartbeatLossPropertyKey = "js7.TEST." + SecretStringGenerator.randomString()
  sys.props(testHeartbeatLossPropertyKey) = "false"

  final def runControllerAndBackup()(body: (DirectoryProvider, RunningController, DirectoryProvider, RunningController, ClusterSetting) => Unit)
  : Unit =
    withControllerAndBackup() { (primary, backup, clusterSetting) =>
      runControllers(primary, backup) { (primaryController, backupController) =>
        body(primary, primaryController, backup, backupController, clusterSetting)
      }
    }

  final def withControllerAndBackup()
    (body: (DirectoryProvider, DirectoryProvider, ClusterSetting) => Unit)
  : Unit =
    withControllerAndBackupWithoutAgents() { (primary, backup, setting) =>
      primary.runAgents() { _ =>
        body(primary, backup, setting)
      }
    }

  val userAndPassword = UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD"))

  final def withControllerAndBackupWithoutAgents()
    (body: (DirectoryProvider, DirectoryProvider, ClusterSetting) => Unit)
  : Unit =
    withCloser { implicit closer =>
      val testName = ControllerClusterForScalaTest.this.getClass.getSimpleName
      val agentPorts = findFreeTcpPorts(agentPaths.size)
      val primary = new DirectoryProvider(agentPaths, items, testName = Some(s"$testName-Primary"),
        controllerConfig = combine(
          primaryControllerConfig,
          configIf(configureClusterNodes, config"""
            js7.journal.cluster.nodes = {
              Primary: "http://127.0.0.1:$primaryControllerPort"
              Backup: "http://127.0.0.1:$backupControllerPort"
            }"""),
          config"""
            js7.journal.cluster.heartbeat = ${clusterTiming.heartbeat.toSeconds}s
            js7.journal.cluster.heartbeat-timeout = ${clusterTiming.heartbeatTimeout.toSeconds}s
            js7.journal.cluster.watches = [ "http://127.0.0.1:${agentPorts.head}" ]
            js7.journal.cluster.TEST-HEARTBEAT-LOSS = "$testHeartbeatLossPropertyKey"
            js7.journal.release-events-delay = 0s
            js7.journal.remove-obsolete-files = $removeObsoleteJournalFiles
            js7.auth.users.TEST-USER.password = "plain:TEST-PASSWORD"
            js7.auth.users.Controller.password = "plain:PRIMARY-CONTROLLER-PASSWORD"
            js7.auth.cluster.password = "BACKUP-CONTROLLER-PASSWORD" """),
        agentPorts = agentPorts,
        agentConfig = config"""js7.job.execution.signed-script-injection-allowed = on"""
      ).closeWithCloser

      val backup = new DirectoryProvider(Nil, Nil, testName = Some(s"$testName-Backup"),
        controllerConfig = combine(
          backupControllerConfig,
          config"""
            js7.journal.cluster.node.is-backup = yes
            js7.journal.cluster.TEST-HEARTBEAT-LOSS = "$testHeartbeatLossPropertyKey"
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

      for (a <- primary.agents) a.writeExecutable(TestPathExecutable, shellScript)

      val setting = ClusterSetting(
        Map(
          primaryId -> Uri(s"http://127.0.0.1:$primaryControllerPort"),
          backupId -> Uri(s"http://127.0.0.1:$backupControllerPort")),
        activeId = primaryId,
        primary.subagentRefs.take(1).map(o => ClusterSetting.Watch(o.uri)),
        clusterTiming)

      body(primary, backup, setting)
    }

  protected final def runControllers(primary: DirectoryProvider, backup: DirectoryProvider)
    (body: (RunningController, RunningController) => Unit)
  : Unit =
    backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
      primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
        primaryController.eventWatch.await[ClusterCoupled]()
        backupController.eventWatch.await[ClusterCoupled]()
        body(primaryController, backupController)
      }
    }

  /** Simulate a kill via ShutDown(failOver) - still writes new snapshot. */
  protected final def simulateKillActiveNode(controller: RunningController): Task[Unit] =
    controller
      .executeCommandAsSystemUser(
        ShutDown(clusterAction = Some(ShutDown.ClusterAction.Failover)))
      .map(_.orThrow)
      .flatMap(_ => Task.deferFuture(controller.terminated))
      .map((_: ProgramTermination) => ())
}

object ControllerClusterForScalaTest
{
  val TestPathExecutable = RelativePathExecutable("TEST.cmd")

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
        waitForCondition(9.s, 100.ms)(backupJournalFile.contentString == primaryFile.contentString)
        assert(backupJournalFile.contentString == primaryFile.contentString)
        assert(backupJournalFile.byteArray == primaryFile.byteArray)
      }
    }
  }
}
