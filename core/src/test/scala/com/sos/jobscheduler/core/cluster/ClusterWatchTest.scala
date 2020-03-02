package com.sos.jobscheduler.core.cluster

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.core.cluster.ClusterWatch._
import com.sos.jobscheduler.data.cluster.ClusterEvent.{BackupNodeAppointed, ClusterCoupled, FailedOver, FollowerLost, FollowingStarted, SwitchedOver}
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterState}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.JournalPosition
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.master.MasterId
import monix.execution.Scheduler.Implicits.global
import monix.execution.schedulers.TestScheduler
import org.scalatest.FreeSpec
import scala.collection.immutable._

/**
  * @author Joacim Zschimmer
  */
final class ClusterWatchTest extends FreeSpec
{
  private val primary = Uri("PRIMARY")
  private val backup = Uri("BACKUP")
  private val failedAt = JournalPosition(0, 0)

  "ClusterWatch" - {
    lazy val scheduler = TestScheduler()
    var clusterState: ClusterState = ClusterState.Sole(primary)
    lazy val watch = new ClusterWatch(MasterId("MASTER"), scheduler)

    "Early heartbeat" in {
      scheduler.tick(1.s)
      assert(watch.heartbeat(primary, clusterState).await(99.s) == Right(Completed))
      assert(watch.isActive(primary).await(99.s).orThrow)
    }

    "Late heartbeat" in {
      scheduler.tick(11.s)
      assert(watch.heartbeat(primary, clusterState).await(99.s) == Right(Completed))
      assert(watch.isActive(primary).await(99.s).orThrow)
    }

    "Coupling" in {
      scheduler.tick(11.s)
      assert(applyEvent(primary, BackupNodeAppointed(backup) :: FollowingStarted(backup) :: ClusterCoupled :: Nil) == Right(Completed))
      assert(watch.isActive(primary).await(99.s).orThrow)
    }

    "Late heartbeat after coupled" in {
      scheduler.tick(11.s)
      assert(watch.heartbeat(primary, clusterState).await(99.s) == Right(Completed))
      assert(watch.isActive(primary).await(99.s).orThrow)
    }

    "Heartbeat with different ClusterState from same active node is accepted" in {
      val reportedClusterState = ClusterState.Decoupled(primary, backup, None)
      assert(watch.heartbeat(primary, reportedClusterState).await(99.s) == Right(Completed))
      assert(watch.get.await(99.s) == Right(reportedClusterState))

      // Restore ClusterState for following tests
      assert(watch.heartbeat(primary, clusterState).await(99.s) == Right(Completed))
      assert(watch.get.await(99.s) == Right(clusterState))
    }

    "Heartbeat from wrong node is rejected" in {
      assert(watch.heartbeat(backup, clusterState).await(99.s) ==
        Left(ClusterWatchHeartbeatFromInactiveNodeProblem(backup, clusterState)))

      locally {
        assert(watch.heartbeat(backup, ClusterState.Coupled(activeUri = backup, primary, None)).await(99.s) ==
          Left(ClusterWatchHeartbeatFromInactiveNodeProblem(backup, clusterState)))
      }

      assert(watch.get.await(99.s) == Right(clusterState))
    }

    "Heartbeat must not change active URI" in {
      // The inactive primary node should not send a heartbeat
      val badCoupled = ClusterState.Coupled(activeUri = backup, primary, None)
      assert(watch.heartbeat(primary, badCoupled).await(99.s) ==
        Left(InvalidClusterWatchHeartbeatProblem(primary, badCoupled)))
      assert(watch.get.await(99.s) == Right(clusterState))
    }

    "FailedOver before heartbeat loss is rejected" in {
      scheduler.tick(1.s)
      assert(applyEvent(backup, FailedOver(primary, backup, failedAt) :: Nil) ==
        Left(ClusterWatchHeartbeatFromInactiveNodeProblem(backup, clusterState)))
      assert(watch.isActive(primary).await(99.s).orThrow)
    }

    "FailedOver after heartbeat loss" in {
      scheduler.tick(11.s)
      assert(applyEvent(backup, FailedOver(primary, backup, failedAt) :: Nil) == Right(Completed))
      assert(watch.isActive(backup).await(99.s).orThrow)
    }

    "FailedOver to node without heartbeat" in {
      pending
      // FIXME ClusterWatch soll FailedOver ablehnen, wenn der zu aktivierende Knoten keinen aktuellen Herzschlag hat ?
      //  Oder FailedOver soll scheitern, wenn nicht eine absolute Mehrheit der ClusterWatch einen aktuellen Herzschlag
      //  für den zu aktivierenden Knoten hat.
    }

    "Coupled" in {
      assert(applyEvent(backup, FollowingStarted(primary) :: ClusterCoupled :: Nil) == Right(Completed))
    }

    "SwitchedOver before heartbeat" in {
      scheduler.tick(1.s)
      assert(applyEvent(backup, SwitchedOver(primary) :: Nil) == Right(Completed))
      assert(watch.isActive(primary).await(99.s).orThrow)
    }

    "SwitchedOver after heartbeat loss" in {
      scheduler.tick(11.s)
      assert(applyEvent(primary, FollowingStarted(backup) :: ClusterCoupled :: SwitchedOver(backup) :: Nil) == Right(Completed))
      assert(watch.isActive(backup).await(99.s).orThrow)
    }

    "SwitchedOver from inactive node" in {
      assert(applyEvent(backup, FollowingStarted(primary) :: ClusterCoupled :: Nil) == Right(Completed))
      assert(watch.isActive(backup).await(99.s).orThrow)
      assert(applyEvent(primary, SwitchedOver(primary) :: Nil) ==
        Left(ClusterWatchHeartbeatFromInactiveNodeProblem(primary, clusterState)))
    }

    "applyEvents after event loss" in {
      assert(watch.get.await(99.s) == Right(clusterState))
      assert(watch.get.await(99.s) == Right(ClusterState.Coupled(backup, primary, None)))

      // We test the loss of a FollowerLost event, and then apply ClusterCoupled
      val lostEvent = FollowerLost(primary)
      val decoupled = ClusterState.Decoupled(backup, primary, None)
      assert(clusterState.applyEvent(NoKey <-: lostEvent) == Right(decoupled))

      val nextEvent = FollowingStarted(primary)
      val preparedToBeCoupled = ClusterState.PreparedToBeCoupled(backup, primary)
      assert(decoupled.applyEvent(NoKey <-: nextEvent) == Right(preparedToBeCoupled))

      assert(watch.applyEvents(backup, nextEvent :: Nil, preparedToBeCoupled).await(99.s) == Right(Completed))
      assert(watch.get.await(99.s) == Right(preparedToBeCoupled))
    }

    def applyEvent(from: Uri, events: Seq[ClusterEvent]): Checked[Completed] = {
      val expectedClusterState = clusterState.applyEvents(events.map(NoKey <-: _)).orThrow
      val response = watch.applyEvents(from, events, expectedClusterState).await(99.s)
      for (_ <- response) {
        clusterState = expectedClusterState
      }
      assert(watch.get.await(99.s) == Right(clusterState))
      response
    }
  }
}
