package js7.tests.cluster.controller

import fs2.Stream
import js7.base.problem.Checked.Ops
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.cluster.{ClusterEvent, ClusterTiming}
import js7.data.controller.ControllerCommand.TakeSnapshot
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.tests.cluster.controller.ControllerClusterTester.*
import js7.tests.testenv.ProgramEnvTester.assertEqualJournalFiles

final class ReplicatingControllerClusterTest extends ControllerClusterTester:
  protected override val clusterTiming = ClusterTiming(heartbeat = 1.s, heartbeatTimeout = 5.s)

  "Cluster replicates journal files properly" in:
    withControllerAndBackup() { (primary, _, backup, _, _) =>
      val primaryController = primary.newController()
      primaryController.waitUntilReady()
      primaryController.runOrder(FreshOrder(OrderId("🔶"), TestWorkflow.path))

      backup.runController(dontWaitUntilReady = true) { _ =>
        primaryController.eventWatch.await[ClusterEvent.ClusterCouplingPrepared]()

        primaryController.eventWatch.await[ClusterEvent.ClusterCoupled]()
        assert(primaryController.journalActorState.isRequiringClusterAcknowledgement)

        primaryController.runOrder(FreshOrder(OrderId("🔷"), TestWorkflow.path))

        assertEqualJournalFiles(primary.controllerEnv, backup.controllerEnv, n = 1)

        primaryController.api.executeCommand(TakeSnapshot).await(99.s).orThrow
        assertEqualJournalFiles(primary.controllerEnv, backup.controllerEnv, n = 1)

        primaryController.runOrder(FreshOrder(OrderId("🟦"), TestWorkflow.path))
        assertEqualJournalFiles(primary.controllerEnv, backup.controllerEnv, n = 1)

        simulateKillActiveNode(primaryController).await(99.s)
        primaryController.terminate().await(99.s)
      }
      // Test may fail here if computer is slow, and backup controller failed over before termination !!!
      assertEqualJournalFiles(primary.controllerEnv, backup.controllerEnv, n = 1)

      // RESTART

      backup.runController(dontWaitUntilReady = true) { backupController =>
        primary.runController() { primaryController =>
          primaryController.eventWatch.await[ClusterCoupled](
            after = primaryController.eventWatch.fileEventIds.last)

          val lastEventId = primaryController.eventWatch.lastAddedEventId
          primaryController.runOrder(FreshOrder(OrderId("🔹"), TestWorkflow.path))
          primaryController.eventWatch.await[OrderFinished](_.key == OrderId("🔹"), after = lastEventId)
          backupController.eventWatch.await[OrderFinished](_.key == OrderId("🔹"), after = lastEventId)

          // Check acknowledgement of empty event list
          primaryController.api.addOrders(Stream.empty).await(99.s).orThrow  // Emits no events
        }
      }
    }
