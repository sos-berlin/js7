package js7.tests.cluster.controller

import io.circe.syntax.EncoderOps
import java.nio.file.Files.size
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.cluster.ClusterCommon.{ClusterWatchAgreedToActivation, ClusterWatchDisagreedToActivation}
import js7.cluster.ClusterNode.RestartAfterJournalTruncationException
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterSwitchedOver, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.{Coupled, FailedOver}
import js7.data.controller.ControllerCommand.{ClusterSwitchOver, ShutDown}
import js7.data.controller.ControllerEvent
import js7.data.controller.ControllerEvent.ControllerTestEvent
import js7.data.event.*
import js7.data.event.KeyedEvent.NoKey
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.NumberValue
import js7.journal.files.JournalFiles
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.tests.cluster.controller.ControllerClusterTester.*
import js7.tests.cluster.controller.FailoverControllerClusterTest.*
import js7.tests.testenv.ProgramEnvTester.assertEqualJournalFiles
import js7.tester.ScalaTestUtils.awaitAndAssert
import monix.execution.Scheduler.Implicits.traced
import scala.concurrent.duration.Deadline.now

abstract class FailoverControllerClusterTest protected extends ControllerClusterTester:
  override protected def primaryControllerConfig =
    // Short timeout because something blocks web server shutdown occasionally
    config"""js7.web.server.shutdown-timeout = 0.5s"""
      .withFallback(super.primaryControllerConfig)

  protected final def test(addNonReplicatedEvents: Boolean = false): Unit =
    sys.props(testHeartbeatLossPropertyKey) = "false"
    withControllerAndBackup() { (primary, _, backup, _, clusterSetting) =>
      var primaryController = primary.newController()
      var backupController = backup.newController()

      primaryController.eventWatch.await[ClusterWatchRegistered]()
      primaryController.eventWatch.await[ClusterCoupled]()

      val since = now
      val sleepWhileFailing = clusterTiming.activeLostTimeout + 1.s
      val orderId = OrderId("💥")
      primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path, arguments = Map(
        "sleep" -> NumberValue(sleepWhileFailing.toSeconds))))
      primaryController.eventWatch.await[OrderProcessingStarted](_.key == orderId)
      backupController.eventWatch.await[OrderProcessingStarted](_.key == orderId)
      // KILL PRIMARY
      primaryController.api.executeCommand(ShutDown(clusterAction = Some(ShutDown.ClusterAction.Failover)))
        .await(99.s).orThrow
      primaryController.terminated await 99.s
      primaryController.stop.await(99.s)
      logger.info("💥 Controller shut down with backup fail-over while script is running 💥")
      assert(since.elapsed < sleepWhileFailing, "— The Controller should have terminated while the shell script runs")

      val Stamped(failedOverEventId, _, NoKey <-: failedOver) =
        backupController.eventWatch.await[ClusterFailedOver](_.key == NoKey).head
      assert(failedOver.failedAt.fileEventId == backupController.eventWatch.fileEventIds.last ||
             failedOver.failedAt.fileEventId == backupController.eventWatch.fileEventIds.dropRight(1).last)
      val expectedFailedFile = primaryController.conf.journalLocation.file(failedOver.failedAt.fileEventId)
      assert(failedOver.failedAt.position == size(expectedFailedFile))

      awaitAndAssert(backupController.clusterState.await(99.s).isInstanceOf[FailedOver])  // Is a delay okay ???
      assert(backupController.clusterState.await(99.s) ==
        FailedOver(clusterSetting.copy(activeId = backupId), failedOver.failedAt))

      backupController.eventWatch.await[OrderFinished](_.key == orderId, after = failedOverEventId)

      if addNonReplicatedEvents then
        // Add a dummy event to simulate an Event written by the primary
        // but not replicated and acknowledged when failing over.
        // The primary truncates the journal file according to the position of FailedOver.
        val line = Stamped[KeyedEvent[ControllerEvent]](EventId.MaxValue - 2, ControllerTestEvent)
          .asJson.compactPrint + "\n"
        JournalFiles.listJournalFiles(primary.controllerEnv.stateDir / "controller")
          .last.file
          .append(line)

      primaryController = primary.newController()
      if addNonReplicatedEvents then
        // Provisional fix lets Controller terminate
        val termination = primaryController.terminated.await(99.s)
        assert(termination.restart)
        try
          primaryController.stop.await(99.s)
          fail("RestartAfterJournalTruncationException expected")
        catch { case t: RestartAfterJournalTruncationException =>
          logger.info(t.toString)
        }
        primaryController = primary.newController()
      primaryController.eventWatch.await[ClusterCoupled](after = failedOverEventId)
      backupController.eventWatch.await[ClusterCoupled](after = failedOverEventId)
      assertEqualJournalFiles(primary.controllerEnv, backup.controllerEnv, n = 1)

      backupController.api.executeCommand(ClusterSwitchOver()).await(99.s).orThrow
      val recoupledEventId = primaryController.eventWatch.await[ClusterSwitchedOver](after = failedOverEventId).head.eventId

      backupController.terminated await 99.s
      backupController.stop.await(99.s)

      backupController = backup.newController()

      backupController.eventWatch.await[ClusterCoupled](after = recoupledEventId)
      primaryController.eventWatch.await[ClusterCoupled](after = recoupledEventId)

      val whenClusterWatchAgrees = backupController.testEventBus.when[ClusterWatchAgreedToActivation.type].runToFuture
      val whenClusterWatchDoesNotAgree = backupController.testEventBus.when[ClusterWatchDisagreedToActivation.type].runToFuture
      sys.props(testHeartbeatLossPropertyKey) = "true"

      // When heartbeat from passive to active node is broken, the ClusterWatch will nonetheless not agree to a failover
      val stillCoupled = Coupled(clusterSetting)
      assert(primaryController.clusterState.await(99.s) == stillCoupled)
      assert(backupController.clusterState.await(99.s) == stillCoupled)

      whenClusterWatchDoesNotAgree await 99.s
      assert(!whenClusterWatchAgrees.isCompleted)
      assert(primaryController.clusterState.await(99.s) == stillCoupled)
      assert(backupController.clusterState.await(99.s) == stillCoupled)

      primaryController.stop.await(99.s)
      backupController.stop.await(99.s)
    }


object FailoverControllerClusterTest:
  private val logger = Logger[this.type]

final class NonTruncatingFailoverControllerClusterTest extends FailoverControllerClusterTest:
  "Failover and recouple" in:
    test()

final class TruncatingFailoverControllerClusterTest extends FailoverControllerClusterTest:
  "Failover and recouple with non-replicated (truncated) events" in:
    test(addNonReplicatedEvents = true)
