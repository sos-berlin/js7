package js7.tests.controller.cluster

import js7.base.auth.UserId
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.controller.data.ControllerCommand.TakeSnapshot
import js7.data.cluster.ClusterEvent
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.tests.controller.cluster.ControllerClusterTester._
import js7.tests.testenv.ControllerClusterForScalaTest.assertEqualJournalFiles
import monix.execution.Scheduler.Implicits.global

final class ReplicatingClusterTest extends ControllerClusterTester
{
  "Cluster replicates journal files properly" in {
    withControllerAndBackup() { (primary, backup) =>
      val primaryController = primary.startController(httpPort = Some(primaryControllerPort)) await 99.s
      primaryController.waitUntilReady()
      primaryController.runOrder(FreshOrder(OrderId("🔶"), TestWorkflow.path))

      val backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s
      primaryController.eventWatch.await[ClusterEvent.ClusterCouplingPrepared]()

      primaryController.eventWatch.await[ClusterEvent.ClusterCoupled]()
      assert(primaryController.journalActorState.isRequiringClusterAcknowledgement)

      primaryController.runOrder(FreshOrder(OrderId("🔷"), TestWorkflow.path))

      assertEqualJournalFiles(primary.controller, backup.controller, n = 1)

      primaryController.executeCommandAsSystemUser(TakeSnapshot).await(99.s).orThrow
      assertEqualJournalFiles(primary.controller, backup.controller, n = 1)

      primaryController.runOrder(FreshOrder(OrderId("🔵"), TestWorkflow.path))
      assertEqualJournalFiles(primary.controller, backup.controller, n = 1)

      simulateKillActiveNode(primaryController) await 99.s
      backupController.terminate() await 99.s
      assertEqualJournalFiles(primary.controller, backup.controller, n = 1)

      // RESTART

      backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { backupController =>
        primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
          // Recoupling may take a short time
          waitForCondition(10.s, 10.ms)(primaryController.journalActorState.isRequiringClusterAcknowledgement)
          assert(primaryController.journalActorState.isRequiringClusterAcknowledgement)

          val lastEventId = primaryController.eventWatch.lastAddedEventId
          primaryController.runOrder(FreshOrder(OrderId("🔹"), TestWorkflow.path))
          primaryController.eventWatch.await[OrderFinished](_.key == OrderId("🔹"), after = lastEventId)
          backupController.eventWatch.await[OrderFinished](_.key == OrderId("🔹"), after = lastEventId)

          // Check acknowledgement of empty event list
          primaryController.httpApi.login_(Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD"))).await(99.s)
          primaryController.httpApi.addOrders(Nil).await(99.s)  // Emits no events
        }
      }
    }
  }
}
