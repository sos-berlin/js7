package js7.tests.subagent

import cats.effect.{IO, ResourceIO}
import js7.agent.{RunningAgent, TestAgent}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.copyDirectoryContent
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AllocatedForJvm.useSync
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost, ClusterSettingUpdated}
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, OrderOutcome}
import js7.data.subagent.Problems.ProcessLostDueSubagentUriChangeProblem
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.SemaphoreJob
import js7.tests.subagent.SubagentMoveInClusterTest.*
import js7.tests.testenv.{ControllerAgentForScalaTest, DirectorEnv}

final class SubagentMoveInClusterTest
  extends OurTestSuite, ControllerAgentForScalaTest /*, SubagentTester*/:

  override protected val controllerConfig = config"""
    js7.auth.agents.AGENT = "${agentPath.toString}-PASSWORD"
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
  """.withFallback(super.controllerConfig)

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = true
    js7.journal.cluster.heartbeat = 1s
    js7.journal.cluster.heartbeat-timeout = 3s
  """.withFallback(super.agentConfig)

  protected val agentPaths = Nil

  private val primarySubagentId = SubagentId("PRIMARY-SUBAGENT")
  private lazy val primarySubagentItem = SubagentItem(
    primarySubagentId, agentPath, findFreeLocalUri(), disabled = true)

  private val backupSubagentId = SubagentId("BACKUP-SUBAGENT")
  private lazy val backupSubagentItem = SubagentItem(
    backupSubagentId, agentPath, findFreeLocalUri())
  private lazy val newBackupSubagentItem =
    backupSubagentItem.copy(uri = findFreeLocalUri())

  protected lazy val items = Nil

  "Restart Subagent at another URI" in:
    lazy val primaryDirectorResource: ResourceIO[(DirectorEnv, RunningAgent)] =
      directoryProvider
        .directorEnvResource(
          primarySubagentItem,
          otherSubagentIds = Seq(backupSubagentId, newBackupSubagentItem.id))
        .flatMap: env =>
          env.directorResource.map(env -> _)

    lazy val backupDirectorEnvResource: ResourceIO[DirectorEnv] =
      directoryProvider.directorEnvResource(
        backupSubagentItem,
        otherSubagentIds = Seq(primarySubagentId),
        isClusterBackup = true)

    lazy val newBackupDirectorEnvResource: ResourceIO[DirectorEnv] =
      directoryProvider.directorEnvResource(
        newBackupSubagentItem,
        otherSubagentIds = Seq(primarySubagentId),
        isClusterBackup = true,
        suffix = "-NEW")

    updateItems(
      AgentRef(
        agentPath,
        directors = Seq(primarySubagentId, backupSubagentId),
        itemRevision = None),
      primarySubagentItem,
      backupSubagentItem)
    updateVersionedItems(VersionId("v1"), Seq(workflow))

    // Provide the new backup Director environment, but start the Director later
    lazy val newBackupDirectorEnvAllocated = newBackupDirectorEnvResource.toAllocated.await(99.s)
    lazy val newBackupEnv = newBackupDirectorEnvAllocated.allocatedThing
    val aOrderId = OrderId("A-MOVE-SUBAGENT")

    primaryDirectorResource.useSync(99.s): (_, primaryDirector) =>
      backupDirectorEnvResource.useSync(99.s): backupEnv =>
        val backupDirector = TestAgent(backupEnv.directorResource.toAllocated.await(99.s))
        var eventId = 0L
        var directorEventId = 0L
        primaryDirector.eventWatch.await[ClusterCoupled]()
        TestSemaphoreJob.continue()
        controller.runOrder(FreshOrder(OrderId("A-ORDER"), workflow.path))

        eventId = eventWatch.lastAddedEventId
        locally:
          controller.api.addOrder(FreshOrder(aOrderId, workflow.path)).await(99.s).orThrow
          val processingStarted = eventWatch
            .await[OrderProcessingStarted](_.key == aOrderId, after = eventId).head.value.event
          assert(processingStarted == OrderProcessingStarted(backupSubagentItem.id))
          eventWatch.await[OrderStdoutWritten](_.key == aOrderId, after = eventId)
          // aOrderId is waiting for semaphore

        eventId = eventWatch.lastAddedEventId
        directorEventId = primaryDirector.eventWatch.lastAddedEventId
        controller.api.updateUnsignedSimpleItems(Seq(newBackupSubagentItem)).await(99.s).orThrow
        primaryDirector.eventWatch.await[ClusterPassiveLost](after = directorEventId)
        primaryDirector.eventWatch.await[ClusterSettingUpdated](after = directorEventId)

        sleep(5.s)
        // While sleeping, the following message should be logged:
        // A passive cluster node wanted to couple but http://localhost:... does not respond

        copyDirectoryContent(backupEnv.stateDir, newBackupEnv.stateDir)

        newBackupDirectorEnvAllocated.useSync(99.s): newBackupEnv =>
          newBackupEnv.directorResource.useSync(99.s): _ =>
            // Now, both old and new backup nodes are running

            sleep(5.s)
            // While sleeping, the following message should be logged:
            // ClusterPrepareCoupling command failed with ... Another passive cluster node wanted to couple
            backupDirector.stopThis.await(99.s)
            backupEnv.close()

            primaryDirector.eventWatch.await[ClusterCoupled](after = directorEventId)

            val aProcessed = eventWatch.await[OrderProcessed](_.key == aOrderId, after = eventId).head
            assert(aProcessed.value.event ==
              OrderProcessed.processLost(ProcessLostDueSubagentUriChangeProblem))

            // After ProcessLost at previous Subagent aOrderId restarts at current Subagent
            TestSemaphoreJob.continue(1) // aOrder still runs on bareSubagent (but it is ignored)
            TestSemaphoreJob.continue(1)
            val a2Processed = eventWatch
              .await[OrderProcessed](_.key == aOrderId, after = aProcessed.eventId)
              .head.value.event
            assert(a2Processed == OrderProcessed(OrderOutcome.succeeded))

            eventWatch.await[OrderFinished](_.key == aOrderId, after = eventId)

            locally:
              // Start another order
              val bOrderId = OrderId("B-MOVE-SUBAGENT")
              TestSemaphoreJob.continue(1)
              controller.api.addOrder(FreshOrder(bOrderId, workflow.path)).await(99.s).orThrow
              val bStarted = eventWatch.await[OrderProcessingStarted](_.key == bOrderId, after = eventId)
                .head.value.event
              assert(bStarted == OrderProcessingStarted(newBackupSubagentItem.id))

              eventWatch.await[OrderStdoutWritten](_.key == bOrderId, after = eventId)

              eventWatch.await[OrderProcessed](_.key == bOrderId, after = eventId).head.value.event
              val bProcessed = eventWatch.await[OrderProcessed](_.key == bOrderId, after = eventId)
                .head.value.event
              assert(bProcessed == OrderProcessed(OrderOutcome.succeeded))
              eventWatch.await[OrderFinished](_.key == bOrderId, after = eventId)


object SubagentMoveInClusterTest:
  private val agentPath = AgentPath("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW"),
    Seq(
      TestSemaphoreJob.execute(agentPath)))

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
