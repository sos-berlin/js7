package js7.tests.controller.cluster

import js7.base.auth.UserId
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.data.cluster.ClusterEvent
import js7.data.controller.ControllerCommand.TakeSnapshot
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.tests.controller.cluster.ControllerClusterTester.*
import js7.tests.testenv.ControllerClusterForScalaTest.assertEqualJournalFiles
import monix.execution.Scheduler.Implicits.traced

final class ReplicatingControllerClusterTest extends ControllerClusterTester
{
  "Cluster replicates journal files properly" in {
    withControllerAndBackup() { (primary, backup, _) =>
      val primaryController = primary.newController(httpPort = Some(primaryControllerPort))
      primaryController.waitUntilReady()
      primaryController.runOrder(FreshOrder(OrderId("🔶"), TestWorkflow.path))

      backup.runController(httpPort = Some(backupControllerPort), dontWaitUntilReady = true) { _ =>
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
        primaryController.terminate().await(99.s)
      }
      // Test may fail here if computer is slow, and backup controller failed over before termination !!!
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