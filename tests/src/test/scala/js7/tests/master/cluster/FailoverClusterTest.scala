package js7.tests.master.cluster

import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.common.guice.GuiceImplicits.RichInjector
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.core.event.journal.files.JournalFiles.JournalMetaOps
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterSwitchedOver}
import js7.data.cluster.ClusterNodeId
import js7.data.cluster.ClusterState.{Coupled, FailedOver}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event._
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.master.cluster.PassiveClusterNode
import js7.master.configuration.MasterConfiguration
import js7.master.data.MasterCommand.{ClusterSwitchOver, ShutDown}
import js7.tests.master.cluster.MasterClusterTester._
import java.nio.file.Files.size
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.Deadline.now

final class FailoverClusterTest extends MasterClusterTester
{
  "Failover and recouple" in {
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      var primaryMaster = primary.startMaster(httpPort = Some(primaryHttpPort)) await 99.s
      var backupMaster = backup.startMaster(httpPort = Some(backupHttpPort)) await 99.s
      val primaryId = ClusterNodeId("Primary")
      val backupId = ClusterNodeId("Backup")
      val idToUri = Map(
        primaryId -> primaryMaster.localUri,
        backupId -> backupMaster.localUri)
      primaryMaster.eventWatch.await[ClusterCoupled]()

      val t = now
      val sleepWhileFailing = 10.s  // Failover takes some seconds anyway
      val orderId = OrderId("💥")
      primaryMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path, arguments = Map(
        "SLEEP" -> sleepWhileFailing.toSeconds.toString)))
      primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)
      backupMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)
      // KILL PRIMARY
      primaryMaster.executeCommandAsSystemUser(ShutDown(clusterAction = Some(ShutDown.ClusterAction.Failover)))
        .await(99.s).orThrow
      primaryMaster.terminated await 99.s
      assert(now < t + sleepWhileFailing, "The shell script should run while Master fails")

      val Stamped(failedOverEventId, _, NoKey <-: failedOver) =
        backupMaster.eventWatch.await[ClusterFailedOver](_.key == NoKey).head
      assert(failedOver.failedAt.fileEventId == backupMaster.eventWatch.fileEventIds.last ||
             failedOver.failedAt.fileEventId == backupMaster.eventWatch.fileEventIds.dropRight(1).last)
      val expectedFailedFile = primaryMaster.injector.instance[MasterConfiguration].journalMeta.file(failedOver.failedAt.fileEventId)
      assert(failedOver.failedAt.position == size(expectedFailedFile))

      waitForCondition(10.s, 10.ms)(backupMaster.clusterState.await(99.s).isInstanceOf[FailedOver])  // Is a delay okay ???
      assert(backupMaster.clusterState.await(99.s) == FailedOver(idToUri, backupId, failedOver.failedAt))

      backupMaster.eventWatch.await[OrderFinished](_.key == orderId, after = failedOverEventId)

      primaryMaster = primary.startMaster(httpPort = Some(primaryHttpPort)) await 99.s
      primaryMaster.eventWatch.await[ClusterCoupled](after = failedOverEventId)
      backupMaster.eventWatch.await[ClusterCoupled](after = failedOverEventId)
      assertEqualJournalFiles(primary.master, backup.master, n = 1)

      backupMaster.executeCommandForTest(ClusterSwitchOver).orThrow
      val recoupledEventId = backupMaster.eventWatch.await[ClusterSwitchedOver](after = failedOverEventId).head.eventId
      primaryMaster.eventWatch.await[ClusterSwitchedOver](after = failedOverEventId)

      backupMaster.terminated await 99.s
      backupMaster = backup.startMaster(httpPort = Some(backupHttpPort)) await 99.s
      backupMaster.eventWatch.await[ClusterCoupled](after = recoupledEventId)
      primaryMaster.eventWatch.await[ClusterCoupled](after = recoupledEventId)

      // When heartbeat from passive to active node is broken, the ClusterWatch will nonetheless not agree to a failover
      val stillCoupled = Coupled(idToUri, primaryId)
      assert(primaryMaster.clusterState.await(99.s) == stillCoupled)
      assert(backupMaster.clusterState.await(99.s) == stillCoupled)

      val whenAgentAgrees = backupMaster.testEventBus.when[PassiveClusterNode.ClusterWatchAgreesToActivation.type].runToFuture
      val whenAgentDoesNotAgree = backupMaster.testEventBus.when[PassiveClusterNode.ClusterWatchDisagreeToActivation.type].runToFuture
      sys.props(testHeartbeatLossPropertyKey) = "true"
      whenAgentDoesNotAgree await 99.s
      assert(!whenAgentAgrees.isCompleted)
      assert(primaryMaster.clusterState.await(99.s) == stillCoupled)
      assert(backupMaster.clusterState.await(99.s) == stillCoupled)

      primaryMaster.terminate() await 99.s
      backupMaster.terminate() await 99.s
    }
  }
}
