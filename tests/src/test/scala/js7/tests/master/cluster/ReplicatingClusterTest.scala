package js7.tests.master.cluster

import js7.base.auth.UserId
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.data.cluster.ClusterEvent
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.master.data.MasterCommand.TakeSnapshot
import js7.tests.master.cluster.MasterClusterTester._
import monix.execution.Scheduler.Implicits.global

final class ReplicatingClusterTest extends MasterClusterTester
{
  "Cluster replicates journal files properly" in {
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      val primaryMaster = primary.startMaster(httpPort = Some(primaryHttpPort)) await 99.s
      primaryMaster.waitUntilReady()
      primaryMaster.runOrder(FreshOrder(OrderId("🔶"), TestWorkflow.path))

      val backupMaster = backup.startMaster(httpPort = Some(backupHttpPort)) await 99.s
      primaryMaster.eventWatch.await[ClusterEvent.ClusterCouplingPrepared]()

      primaryMaster.eventWatch.await[ClusterEvent.ClusterCoupled]()
      assert(primaryMaster.journalActorState.isRequiringClusterAcknowledgement)

      primaryMaster.runOrder(FreshOrder(OrderId("🔷"), TestWorkflow.path))

      assertEqualJournalFiles(primary.master, backup.master, n = 1)

      primaryMaster.executeCommandAsSystemUser(TakeSnapshot).await(99.s).orThrow
      assertEqualJournalFiles(primary.master, backup.master, n = 1)

      primaryMaster.runOrder(FreshOrder(OrderId("🔵"), TestWorkflow.path))
      assertEqualJournalFiles(primary.master, backup.master, n = 1)

      simulateKillActiveNode(primaryMaster) await 99.s
      backupMaster.terminate() await 99.s
      assertEqualJournalFiles(primary.master, backup.master, n = 1)

      // RESTART

      backup.runMaster(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupMaster =>
        primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
          // Recoupling may take a short time
          waitForCondition(10.s, 10.ms)(primaryMaster.journalActorState.isRequiringClusterAcknowledgement)
          assert(primaryMaster.journalActorState.isRequiringClusterAcknowledgement)

          val lastEventId = primaryMaster.eventWatch.lastAddedEventId
          primaryMaster.runOrder(FreshOrder(OrderId("🔹"), TestWorkflow.path))
          primaryMaster.eventWatch.await[OrderFinished](_.key == OrderId("🔹"), after = lastEventId)
          backupMaster.eventWatch.await[OrderFinished](_.key == OrderId("🔹"), after = lastEventId)

          // Check acknowledgement of empty event list
          primaryMaster.httpApi.login_(Some(UserId("TEST") -> SecretString("TEST-PASSWORD"))).await(99.s)
          primaryMaster.httpApi.addOrders(Nil).await(99.s)  // Emits no events
        }
      }
    }
  }
}
