package js7.tests.controller.cluster

import java.nio.file.Files.size
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.common.guice.GuiceImplicits.RichInjector
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.controller.cluster.PassiveClusterNode
import js7.controller.configuration.ControllerConfiguration
import js7.controller.data.ControllerCommand.{ClusterSwitchOver, ShutDown}
import js7.core.event.journal.files.JournalFiles.JournalMetaOps
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, FailedOver}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event._
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.tests.controller.cluster.ControllerClusterTester._
import js7.tests.testenv.ControllerClusterForScalaTest.assertEqualJournalFiles
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.Deadline.now

final class FailoverClusterTest extends ControllerClusterTester
{
  "Failover and recouple" in {
    withControllerAndBackup() { (primary, backup) =>
      var primaryController = primary.startController(httpPort = Some(primaryControllerPort)) await 99.s
      var backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s
      val idToUri = Map(
        primaryId -> primaryController.localUri,
        backupId -> backupController.localUri)
      primaryController.eventWatch.await[ClusterCoupled]()

      val t = now
      val sleepWhileFailing = 10.s  // Failover takes some seconds anyway
      val orderId = OrderId("💥")
      primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path, arguments = Map(
        "SLEEP" -> sleepWhileFailing.toSeconds.toString)))
      primaryController.eventWatch.await[OrderProcessingStarted](_.key == orderId)
      backupController.eventWatch.await[OrderProcessingStarted](_.key == orderId)
      // KILL PRIMARY
      primaryController.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ShutDown.ClusterAction.Failover)))
        .await(99.s).orThrow
      primaryController.terminated await 99.s
      assert(now < t + sleepWhileFailing, "The shell script should run while Controller fails")

      val Stamped(failedOverEventId, _, NoKey <-: failedOver) =
        backupController.eventWatch.await[ClusterFailedOver](_.key == NoKey).head
      assert(failedOver.failedAt.fileEventId == backupController.eventWatch.fileEventIds.last ||
             failedOver.failedAt.fileEventId == backupController.eventWatch.fileEventIds.dropRight(1).last)
      val expectedFailedFile = primaryController.injector.instance[ControllerConfiguration].journalMeta.file(failedOver.failedAt.fileEventId)
      assert(failedOver.failedAt.position == size(expectedFailedFile))

      waitForCondition(10.s, 10.ms)(backupController.clusterState.await(99.s).isInstanceOf[FailedOver])  // Is a delay okay ???
      assert(backupController.clusterState.await(99.s) == FailedOver(idToUri, backupId, failedOver.failedAt))

      backupController.eventWatch.await[OrderFinished](_.key == orderId, after = failedOverEventId)

      primaryController = primary.startController(httpPort = Some(primaryControllerPort)) await 99.s
      primaryController.eventWatch.await[ClusterCoupled](after = failedOverEventId)
      backupController.eventWatch.await[ClusterCoupled](after = failedOverEventId)
      assertEqualJournalFiles(primary.controller, backup.controller, n = 1)

      backupController.executeCommandForTest(ClusterSwitchOver).orThrow
      val recoupledEventId = primaryController.eventWatch.await[ClusterSwitchedOver](after = failedOverEventId).head.eventId

      backupController.terminated await 99.s
      backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s
      backupController.eventWatch.await[ClusterCoupled](after = recoupledEventId)
      primaryController.eventWatch.await[ClusterCoupled](after = recoupledEventId)

      // When heartbeat from passive to active node is broken, the ClusterWatch will nonetheless not agree to a failover
      val stillCoupled = Coupled(idToUri, primaryId)
      assert(primaryController.clusterState.await(99.s) == stillCoupled)
      assert(backupController.clusterState.await(99.s) == stillCoupled)

      val whenAgentAgrees = backupController.testEventBus.when[PassiveClusterNode.ClusterWatchAgreesToActivation.type].runToFuture
      val whenAgentDoesNotAgree = backupController.testEventBus.when[PassiveClusterNode.ClusterWatchDisagreeToActivation.type].runToFuture
      sys.props(testHeartbeatLossPropertyKey) = "true"
      whenAgentDoesNotAgree await 99.s
      assert(!whenAgentAgrees.isCompleted)
      assert(primaryController.clusterState.await(99.s) == stillCoupled)
      assert(backupController.clusterState.await(99.s) == stillCoupled)

      primaryController.terminate() await 99.s
      backupController.terminate() await 99.s
    }
  }
}
