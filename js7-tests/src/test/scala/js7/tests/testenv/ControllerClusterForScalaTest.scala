package js7.tests.testenv

import cats.effect.{Resource, SyncIO}
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Files
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import js7.agent.TestAgent
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.configutils.Configs.*
import js7.base.eventbus.StandardEventBus
import js7.base.generic.SecretString
import js7.base.io.https.HttpsConfig
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.problem.Checked.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsBlocking.BlockingTaskResource
import js7.base.utils.CatsUtils.{Nel, combine}
import js7.base.utils.Closer.syntax.*
import js7.base.utils.Closer.withCloser
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.cluster.watch.ClusterWatchService
import js7.cluster.watch.api.ClusterWatchProblems.ClusterNodeLossNotConfirmedProblem
import js7.common.auth.SecretStringGenerator
import js7.common.configuration.Js7Configuration
import js7.common.message.ProblemCodeMessages
import js7.common.system.ThreadPools
import js7.common.utils.FreeTcpPortFinder.{findFreeTcpPort, findFreeTcpPorts}
import js7.data.agent.AgentPath
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.cluster.{ClusterSetting, ClusterTiming, ClusterWatchId}
import js7.data.controller.ControllerCommand.ShutDown
import js7.data.item.InventoryItem
import js7.data.job.RelativePathExecutable
import js7.data.node.NodeId
import js7.tests.testenv.ControllerClusterForScalaTest.*
import js7.tests.testenv.DirectoryProvider.script
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced as scheduler
import org.jetbrains.annotations.TestOnly
import org.scalactic.source

@TestOnly
trait ControllerClusterForScalaTest
{
  this: org.scalatest.Suite =>

  protected def agentPaths: Seq[AgentPath] = AgentPath("AGENT") :: Nil
  protected def items: Seq[InventoryItem]
  protected def shellScript = script(0.s)

  protected def configureClusterNodes = true
  protected def removeObsoleteJournalFiles = true
  protected final val primaryId = NodeId("Primary")
  protected final val backupId = NodeId("Backup")

  protected def primaryControllerConfig: Config = ConfigFactory.empty
  protected def backupControllerConfig: Config = ConfigFactory.empty

  protected final lazy val primaryControllerPort = findFreeTcpPort()
  protected final lazy val backupControllerPort = findFreeTcpPort()

  val userAndPassword = UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD"))

  protected final lazy val primaryControllerAdmission =
    Admission(Uri(s"http://127.0.0.1:$primaryControllerPort"), Some(userAndPassword))
  protected final lazy val backupControllerAdmission =
    Admission(Uri(s"http://127.0.0.1:$backupControllerPort"), Some(userAndPassword))
  protected final lazy val controllerAdmissions =
    Nel.of(primaryControllerAdmission, backupControllerAdmission)

  protected val clusterTiming = ClusterTiming(heartbeat = 500.ms, heartbeatTimeout = 500.ms)

  protected def clusterWatchConfig: Config =
    ConfigFactory.empty()

  coupleScribeWithSlf4j()
  ProblemCodeMessages.initialize()
  protected final val testHeartbeatLossPropertyKey = "js7.TEST." + SecretStringGenerator.randomString()
  protected final val testAckLossPropertyKey = "js7.TEST." + SecretStringGenerator.randomString()
  sys.props(testAckLossPropertyKey) = "false"
  sys.props(testHeartbeatLossPropertyKey) = "false"

  final def runControllerAndBackup(suppressClusterWatch: Boolean = false)
    (body: (DirectoryProvider, TestController, Vector[TestAgent],
            DirectoryProvider, TestController, Vector[TestAgent], ClusterSetting) => Unit)
  : Unit =
    withControllerAndBackup(suppressClusterWatch) {
      (primary, primaryAgents, backup, backupAgents, clusterSetting) =>
        runControllers(primary, backup) { (primaryController, backupController) =>
          body(
            primary, primaryController, primaryAgents,
            backup, backupController, backupAgents,
            clusterSetting)
        }
    }

  final def withControllerAndBackup(suppressClusterWatch: Boolean = false)
    (body: (DirectoryProvider, Vector[TestAgent], DirectoryProvider, Vector[TestAgent], ClusterSetting) => Unit)
  : Unit =
    withControllerAndBackupWithoutAgents(suppressClusterWatch) {
      (primary, backup, setting) =>
        backup.runAgents() { backupAgents =>
          primary.runAgents() { primaryAgents =>
            body(primary, primaryAgents, backup, backupAgents, setting)
          }
        }
    }

  final def withControllerAndBackupWithoutAgents(suppressClusterWatch: Boolean = false)
    (body: (DirectoryProvider, DirectoryProvider, ClusterSetting) => Unit)
  : Unit =
    withCloser { implicit closer =>
      val testName = ControllerClusterForScalaTest.this.getClass.getSimpleName
      val (agentPorts, backupAgentPorts) = findFreeTcpPorts(2 * agentPaths.size)
        .grouped(2).map(seq => (seq(0), seq(1))).toVector.unzip

      val primary = new DirectoryProvider(
        controllerPort = primaryControllerPort,
        agentPaths, items = items,
        testName = Some(s"$testName-Primary"),
        controllerConfig = combine(
          primaryControllerConfig,
          configIf(configureClusterNodes, config"""
            js7.journal.cluster.nodes = {
              Primary: "http://127.0.0.1:$primaryControllerPort"
              Backup: "http://127.0.0.1:$backupControllerPort"
            }"""),
          config"""
            js7.journal.cluster.heartbeat = ${clusterTiming.heartbeat}
            js7.journal.cluster.heartbeat-timeout = ${clusterTiming.heartbeatTimeout}
            js7.journal.cluster.TEST-HEARTBEAT-LOSS = "$testHeartbeatLossPropertyKey"
            js7.journal.cluster.TEST-ACK-LOSS = "$testAckLossPropertyKey"
            js7.journal.release-events-delay = 0s
            js7.journal.remove-obsolete-files = $removeObsoleteJournalFiles
            js7.auth.cluster.password = "PRIMARY-CONTROLLER-PASSWORD"
            js7.auth.users.Controller.password = "plain:BACKUP-CONTROLLER-PASSWORD"
            js7.auth.users.TEST-USER.password = "plain:TEST-PASSWORD"
            js7.auth.users.TEST-USER.permissions = [ UpdateItem, AgentDirectorForward ]"""),
        agentPorts = agentPorts,
        agentConfig = config"""
          js7.job.execution.signed-script-injection-allowed = on
          js7.journal.cluster.heartbeat = ${clusterTiming.heartbeat}
          js7.journal.cluster.heartbeat-timeout = ${clusterTiming.heartbeatTimeout}
          js7.journal.release-events-delay = 0s
          js7.journal.remove-obsolete-files = $removeObsoleteJournalFiles"""
      ).closeWithCloser

      val backup = new DirectoryProvider(
        controllerPort = backupControllerPort,
        agentPaths, Nil,
        testName = Some(s"$testName-Backup"),
        isBackup = true,
        controllerConfig = combine(
          backupControllerConfig,
          config"""
            js7.journal.cluster.node.is-backup = yes
            js7.journal.cluster.heartbeat = ${clusterTiming.heartbeat}
            js7.journal.cluster.heartbeat-timeout = ${clusterTiming.heartbeatTimeout}
            js7.journal.cluster.TEST-HEARTBEAT-LOSS = "$testHeartbeatLossPropertyKey"
            js7.journal.cluster.TEST-ACK-LOSS = "$testAckLossPropertyKey"
            js7.journal.release-events-delay = 0s
            js7.journal.remove-obsolete-files = $removeObsoleteJournalFiles
            js7.auth.cluster.password = "BACKUP-CONTROLLER-PASSWORD"
            js7.auth.users.Controller.password = "plain:PRIMARY-CONTROLLER-PASSWORD"
            js7.auth.users.TEST-USER.password = "plain:TEST-PASSWORD"
            js7.auth.users.TEST-USER.permissions = [ AgentDirectorForward ]"""),
        agentPorts = backupAgentPorts,
        agentConfig = config"""
          js7.job.execution.signed-script-injection-allowed = on
          js7.journal.cluster.heartbeat = ${clusterTiming.heartbeat}
          js7.journal.cluster.heartbeat-timeout = ${clusterTiming.heartbeatTimeout}
          js7.journal.cluster.node.is-backup = yes
          js7.journal.release-events-delay = 0s
          js7.journal.remove-obsolete-files = $removeObsoleteJournalFiles
          js7.auth.users.Controller.password = "plain:AGENT-PASSWORD" """
      ).closeWithCloser

      // Replicate credentials required for agents
      Files.copy(
        primary.controllerEnv.privateConf,
        backup.controllerEnv.privateConf,
        REPLACE_EXISTING)

      for (a <- primary.agentEnvs ++ backup.agentEnvs) a.writeExecutable(TestPathExecutable, shellScript)

      val setting = ClusterSetting(
        Map(
          primaryId -> Uri(s"http://127.0.0.1:$primaryControllerPort"),
          backupId -> Uri(s"http://127.0.0.1:$backupControllerPort")),
        activeId = primaryId,
        clusterTiming,
        clusterWatchId = None)

      if (suppressClusterWatch)
        body(primary, backup, setting)
      else
        withOptionalClusterWatchService() {
          body(primary, backup, setting.copy(
            clusterWatchId = Some(clusterWatchId)))
        }
    }

  protected final def runControllers(primary: DirectoryProvider, backup: DirectoryProvider)
    (body: (TestController, TestController) => Unit)
  : Unit =
    backup.runController(dontWaitUntilReady = true) { backupController =>
      primary.runController() { primaryController =>
        primaryController.eventWatch.await[ClusterCoupled]()
        backupController.eventWatch.await[ClusterCoupled]()
        body(primaryController, backupController)
      }
    }

  protected final def withOptionalClusterWatchService[A](
    clusterWatchId: ClusterWatchId = ControllerClusterForScalaTest.clusterWatchId)
    (body: => A)
  : A =
    withClusterWatchService(clusterWatchId)((_, _) => body)

  protected final def withClusterWatchService[A](
    clusterWatchId: ClusterWatchId = ControllerClusterForScalaTest.clusterWatchId)
    (body: (ClusterWatchService, StandardEventBus[ClusterNodeLossNotConfirmedProblem]) => A)
  : A =
    ThreadPools
      .standardSchedulerResource[SyncIO](
        s"${getClass.simpleScalaName}-${clusterWatchId.string}",
        Js7Configuration.defaultConfig)
      .use(implicit scheduler => SyncIO(
        clusterWatchServiceResource(clusterWatchId)
          .blockingUse(99.s)(body.tupled)))
      .unsafeRunSync()

  protected final def clusterWatchServiceResource(clusterWatchId: ClusterWatchId)
  : Resource[Task, (ClusterWatchService, StandardEventBus[ClusterNodeLossNotConfirmedProblem])] =
    for {
      eventbus <- Resource.fromAutoCloseable(Task(
        new StandardEventBus[ClusterNodeLossNotConfirmedProblem]))
      clusterWatch <- DirectoryProvider.clusterWatchServiceResource(
        clusterWatchId,
        controllerAdmissions,
        HttpsConfig.empty,
        clusterWatchConfig,
        onUndecidableClusterNodeLoss = {
          case Some(prblm) => Task(eventbus.publish(prblm))
          case None => Task.unit
        })
    } yield (clusterWatch, eventbus)

  /** Simulate a kill via ShutDown(failOver) - still writes new snapshot. */
  protected final def simulateKillActiveNode(controller: TestController): Task[Unit] =
    controller
      .api.executeCommand(
        ShutDown(clusterAction = Some(ShutDown.ClusterAction.Failover)))
      .map(_.orThrow)
      .flatMap(_ => Task.deferFuture(controller.terminated))
      .map((_: ProgramTermination) => ())
}

object ControllerClusterForScalaTest
{
  val TestPathExecutable = RelativePathExecutable("TEST.cmd")
  val clusterWatchId = ClusterWatchId("CLUSTER-WATCH")

  @deprecated
  def assertEqualJournalFiles(
    primary: ProgramEnv.WithFileJournal,
    backup: ProgramEnv.WithFileJournal,
    n: Int)
    (implicit pos: source.Position)
  : Unit =
    ProgramEnvTester.assertEqualJournalFiles(primary, backup, n)
}
