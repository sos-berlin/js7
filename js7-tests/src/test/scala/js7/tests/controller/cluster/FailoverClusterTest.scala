package js7.tests.controller.cluster

import io.circe.syntax.EncoderOps
import java.nio.file.Files.size
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.Ops
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.cluster.ClusterCommon.{ClusterWatchAgreedToActivation, ClusterWatchDisagreedToActivation}
import js7.common.guice.GuiceImplicits.RichInjector
import js7.controller.configuration.ControllerConfiguration
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, FailedOver}
import js7.data.controller.ControllerCommand.{ClusterSwitchOver, ShutDown}
import js7.data.controller.ControllerEvent
import js7.data.controller.ControllerEvent.ControllerTestEvent
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.*
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.StringValue
import js7.journal.files.JournalFiles
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.tests.controller.cluster.ControllerClusterTester.*
import js7.tests.testenv.ControllerClusterForScalaTest.assertEqualJournalFiles
import monix.execution.Scheduler.Implicits.traced
import scala.concurrent.duration.Deadline.now

final class FailoverClusterTest extends ControllerClusterTester
{
  override protected def primaryControllerConfig =
    // Short timeout because something blocks web server shutdown occasionally
    config"""js7.web.server.shutdown-timeout = 0.5s"""
      .withFallback(super.primaryControllerConfig)

  "Failover and recouple" in {
    test()
  }

  "Failover and recouple with non-replicated (truncated) events" in {
    // The intermediate fix will HALT the complete JVM
    // to allow a restart with the truncated journal file
    //pending
    test(addNonReplicatedEvents = true)
  }

  private def test(addNonReplicatedEvents: Boolean = false): Unit = {
    sys.props(testHeartbeatLossPropertyKey) = "false"
    withControllerAndBackup() { (primary, backup, clusterSetting) =>
      var primaryController = primary.startController(httpPort = Some(primaryControllerPort)) await 99.s
      var backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s
      primaryController.eventWatch.await[ClusterCoupled]()

      val since = now
      val sleepWhileFailing = clusterTiming.activeLostTimeout + 1.s
      val orderId = OrderId("💥")
      primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path, arguments = Map(
        "SLEEP" -> StringValue(sleepWhileFailing.toSeconds.toString))))
      primaryController.eventWatch.await[OrderProcessingStarted](_.key == orderId)
      backupController.eventWatch.await[OrderProcessingStarted](_.key == orderId)
      // KILL PRIMARY
      primaryController.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ShutDown.ClusterAction.Failover)))
        .await(99.s).orThrow
      primaryController.terminated await 99.s
      scribe.info("💥 Controller shut down with backup fail-over while script is running 💥")
      assert(since.elapsed < sleepWhileFailing, "— The Controller should have terminated while the shell script runs")

      val Stamped(failedOverEventId, _, NoKey <-: failedOver) =
        backupController.eventWatch.await[ClusterFailedOver](_.key == NoKey).head
      assert(failedOver.failedAt.fileEventId == backupController.eventWatch.fileEventIds.last ||
             failedOver.failedAt.fileEventId == backupController.eventWatch.fileEventIds.dropRight(1).last)
      val expectedFailedFile = primaryController.injector.instance[ControllerConfiguration].journalMeta.file(failedOver.failedAt.fileEventId)
      assert(failedOver.failedAt.position == size(expectedFailedFile))

      waitForCondition(10.s, 10.ms)(backupController.clusterState.await(99.s).isInstanceOf[FailedOver])  // Is a delay okay ???
      assert(backupController.clusterState.await(99.s) ==
        FailedOver(clusterSetting.copy(activeId = backupId), failedOver.failedAt))

      backupController.eventWatch.await[OrderFinished](_.key == orderId, after = failedOverEventId)

      if (addNonReplicatedEvents) {
        // Add a dummy event to simulate an Event written by the primary
        // but not replicated and acknowledged when failing over.
        // The primary truncates the journal file according to the position of FailedOver.
        val line = Stamped[KeyedEvent[ControllerEvent]](EventId.MaxValue - 2, ControllerTestEvent)
          .asJson.compactPrint + "\n"
        JournalFiles.listJournalFiles(primary.controller.stateDir / "controller")
          .last.file
          .append(line)
      }

      primaryController = primary.startController(httpPort = Some(primaryControllerPort)) await 99.s
      if (addNonReplicatedEvents) {
        // Provisional fix lets Controller terminate
        primaryController.terminated await 99.s
        primaryController = primary.startController(httpPort = Some(primaryControllerPort)) await 99.s
        //val tried = Await.ready(primaryController.terminated, 99.s).value.get
        //assert(tried.failed.get.isInstanceOf[RestartAfterJournalTruncationException])
      }
      primaryController.eventWatch.await[ClusterCoupled](after = failedOverEventId)
      backupController.eventWatch.await[ClusterCoupled](after = failedOverEventId)
      assertEqualJournalFiles(primary.controller, backup.controller, n = 1)

      backupController.executeCommandForTest(ClusterSwitchOver).orThrow
      val recoupledEventId = primaryController.eventWatch.await[ClusterSwitchedOver](after = failedOverEventId).head.eventId

      backupController.terminated await 99.s
      backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s
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

      primaryController.terminate() await 99.s
      backupController.terminate() await 99.s
    }
  }
}
