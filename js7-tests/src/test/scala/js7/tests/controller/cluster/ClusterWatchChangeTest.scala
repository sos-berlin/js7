package js7.tests.controller.cluster

import js7.base.log.Logger
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.cluster.ClusterNode.ClusterWatchConfirmed
import js7.cluster.watch.api.ClusterWatchProblems.{ClusterWatchRequestDoesNotMatchProblem, OtherClusterWatchStillAliveProblem}
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterCouplingPrepared, ClusterWatchRegistered}
import js7.data.cluster.{ClusterState, ClusterWatchId}
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.controller.cluster.ClusterWatchChangeTest.*
import monix.execution.Scheduler.Implicits.traced
import scala.concurrent.Promise

final class ClusterWatchChangeTest extends ControllerClusterTester
{
  "Start and stop some ClusterWatch with same or different ClusterWatchIds" in {
    withControllerAndBackup(suppressClusterWatch = true) { (primary, backup, _) =>
      val primaryController = primary.startController(httpPort = Some(primaryControllerPort)) await 99.s
      var whenConfirmed = primaryController.testEventBus.when[ClusterWatchConfirmed].runToFuture

      val backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s

      // ClusterWatch is not required for ClusterCouplingPrepared
      val x = primaryController.eventWatch.await[ClusterCouplingPrepared]().head.eventId
      sleep(3.s)
      // But ClusterWatch is required for ClusterCoupled
      assert(primaryController.eventWatch.allKeyedEvents[ClusterCoupled].isEmpty)

      withClusterWatchService(aClusterWatchId) { a =>
        val z = primaryController.eventWatch.await[ClusterCoupled]().head.eventId
        // ClusterWatchRegistered is expected to immediately follow ClusterCouplingPrepared
        // but we await this only now, to not force this wrong behaviour.
        val y = primaryController.eventWatch.await[ClusterWatchRegistered]().head.eventId
        assert(x < y && y < z)

        val confirmed = whenConfirmed.await(99.s)
        assert(confirmed.command.clusterWatchId == a.clusterWatchId)
        assert(confirmed.command.clusterWatchRunId == a.clusterWatchRunId)
        assert(confirmed.result == Right(()))

        assert(primaryController.controllerState.await(99.s)
          .clusterState.asInstanceOf[ClusterState.Coupled].setting.clusterWatchId
          == Some(aClusterWatchId))

        awaitAndAssert(
          a.clusterState().exists(_.isInstanceOf[ClusterState.Coupled]))
      }

      logger.info("🔵 Same ClusterWatchId again")

      def whenProperRequestIsConfirmed() = primaryController.testEventBus
        .when_[ClusterWatchConfirmed](predicate = confirmed =>
          confirmed.result != Left(ClusterWatchRequestDoesNotMatchProblem) || {
            // bClusterWatchId may confirm the last request already confirmed by aClusterWatchId
            logger.debug(s"Ignore: $confirmed")
            false
          })
        .runToFuture

      whenConfirmed = whenProperRequestIsConfirmed()
      withClusterWatchService(aClusterWatchId) { a =>
        val confirmed = whenConfirmed.await(99.s)
        assert(confirmed.command.clusterWatchId == a.clusterWatchId)
        assert(confirmed.command.clusterWatchRunId == a.clusterWatchRunId)
        assert(confirmed.result == Right(()))
      }

      logger.info("🔵 Different ClusterWatchId")
      whenConfirmed = whenProperRequestIsConfirmed()
      val eventId = primaryController.eventWatch.lastAddedEventId
      withClusterWatchService(bClusterWatchId) { b =>
        // aClusterWatchId has not yet expired
        var confirmed = whenConfirmed.await(99.s)
        assert(confirmed.command.clusterWatchId == b.clusterWatchId)
        assert(confirmed.command.clusterWatchRunId == b.clusterWatchRunId)
        assert(confirmed.result == Left(OtherClusterWatchStillAliveProblem(
          rejectedClusterWatchId = bClusterWatchId,
          requestedClusterWatchId = aClusterWatchId)))

        logger.info("🔵 Wait for expiration of aClusterWatchId")
        assert(primaryController.eventWatch.await[ClusterWatchRegistered](after = eventId)
          .head.value.event.clusterWatchId == b.clusterWatchId)

        logger.info("🔵 Start DUPLICATE ClusterWatch with SAME bClusterWatchId")
        // bClusterWatchId is used twice, non-unique
        val executedPromise = Promise[ClusterWatchConfirmed]()
        val subscription = primaryController.testEventBus
          .subscribe[ClusterWatchConfirmed] { executed =>
            // Await ClusterWatchConfirmed of second bClusterWatchId
            if (executed.command.clusterWatchId == b.clusterWatchId
              && executed.command.clusterWatchRunId != b.clusterWatchRunId
              && executed.result == Right(())) {
              executedPromise.trySuccess(executed)
            }
          }

        // Start a duplicate bClusterWatchId. It's ClusterWatchRunId replaces the first one.
        autoClosing(subscription) { _ =>
          withClusterWatchService(bClusterWatchId) { b =>
            val executed = executedPromise.future.await(99.s)
            assert(executed.command.clusterWatchId == b.clusterWatchId)
            assert(executed.command.clusterWatchRunId == b.clusterWatchRunId)
            assert(executed.result == Right(()))
          }
        }

        logger.info("🔵 Now, the first bClusterWatch is invalid and cannot be used")
        // ClusterWatchCounterpart detects this
        confirmed = primaryController.testEventBus.when[ClusterWatchConfirmed].await(99.s)
        assert(confirmed.command.clusterWatchId == b.clusterWatchId)
        assert(confirmed.command.clusterWatchRunId == b.clusterWatchRunId)

        assert(primaryController.eventWatch.allKeyedEvents[ClusterWatchRegistered].map(_.event) ==
          Seq(
            ClusterWatchRegistered(aClusterWatchId),
            ClusterWatchRegistered(bClusterWatchId)))

        assert(primaryController.controllerState.await(99.s)
          .clusterState.asInstanceOf[ClusterState.Coupled].setting.clusterWatchId
          == Some(bClusterWatchId))
      }

      logger.info("🔵 Start bClusterWatchId again to allow primaryController to terminate")
      // Allow primaryController to terminate by acknowledging ClusterActiveNodeShutdown event.
      whenConfirmed = whenProperRequestIsConfirmed()
      withClusterWatchService(bClusterWatchId) { b =>
        // Wait for ClusterWatchAlive before emitting a ClusterEvent like ClusterActiveNotShutDown,
        // because the latter locks the ClusterState, blocking ClusterWatchAlive,
        // resulting in a deadlock.
        // Duplicate ClusterWatchId is not considered a real world problem !!!
        assert(whenConfirmed.await(99.s).result == Right(()))
        primaryController.terminate().await(99.s)
      }

      backupController.terminate().await(99.s)
    }
  }
}

object ClusterWatchChangeTest
{
  private val logger = Logger[this.type]
  private val aClusterWatchId = ClusterWatchId("A-CLUSTER-WATCH")
  private val bClusterWatchId = ClusterWatchId("B-CLUSTER-WATCH")
}
