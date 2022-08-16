package js7.core.cluster

import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.test.Test
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.common.message.ProblemCodeMessages
import js7.core.cluster.ClusterWatch.*
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.{ClusterEvent, ClusterSetting, ClusterState, ClusterTiming}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournalPosition}
import js7.data.node.NodeId
import monix.execution.Scheduler.Implicits.global
import monix.execution.schedulers.TestScheduler

/**
  * @author Joacim Zschimmer
  */
final class ClusterWatchTest extends Test
{
  ProblemCodeMessages.initialize()

  private val aId = NodeId("A")
  private val bId = NodeId("B")
  private val setting = ClusterSetting(
    Map(
      NodeId("A") -> Uri("http://A"),
      NodeId("B") -> Uri("http://B")),
    activeId = NodeId("A"),
    Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
    ClusterTiming(10.s, 20.s))
  private val failedAt = JournalPosition(EventId(0), 0)

  "ClusterWatch" - {
    lazy val scheduler = TestScheduler()
    var clusterState: ClusterState = ClusterState.NodesAppointed(setting)
    lazy val watch = new ClusterWatch(ControllerId("CONTROLLER"), scheduler)

    "Early heartbeat" in {
      scheduler.tick(1.s)
      assert(watch.heartbeat(aId, clusterState).await(99.s) == Right(Completed))
      assert(watch.isActive(aId).await(99.s).orThrow)
    }

    "Late heartbeat" in {
      scheduler.tick(11.s)
      assert(watch.heartbeat(aId, clusterState).await(99.s) == Right(Completed))
      assert(watch.isActive(aId).await(99.s).orThrow)
    }

    "Coupling" in {
      scheduler.tick(11.s)
      assert(applyEvents(aId, ClusterCouplingPrepared(aId) :: ClusterCoupled(aId) :: Nil) == Right(Completed))
      assert(watch.isActive(aId).await(99.s).orThrow)
    }

    "Late heartbeat after coupled" in {
      scheduler.tick(11.s)
      assert(watch.heartbeat(aId, clusterState).await(99.s) == Right(Completed))
      assert(watch.isActive(aId).await(99.s).orThrow)
    }

    "Heartbeat with different ClusterState from same active node is rejected" in {
      val reportedClusterState = ClusterState.PassiveLost(setting)
      assert(watch.heartbeat(aId, reportedClusterState).await(99.s) ==
        Left(ClusterWatchHeartbeatMismatchProblem(clusterState, reportedClusterState)))
    }

    "Heartbeat from wrong node is rejected" in {
      assert(watch.heartbeat(bId, clusterState).await(99.s) ==
        Left(ClusterWatchInactiveNodeProblem(bId, clusterState, 0.s, "heartbeat Coupled(active A: http://A, passive B: http://B)")))

      locally {
        assert(watch.heartbeat(bId, ClusterState.Coupled(setting.copy(activeId = bId))).await(99.s) ==
          Left(ClusterWatchInactiveNodeProblem(bId, clusterState, 0.s, "heartbeat Coupled(passive A: http://A, active B: http://B)")))
      }

      assert(watch.get.await(99.s) == Right(clusterState))
    }

    "Heartbeat must not change active URI" in {
      // The inactive primary node should not send a heartbeat
      val badCoupled = ClusterState.Coupled(setting.copy(activeId = bId))
      assert(watch.heartbeat(aId, badCoupled).await(99.s) ==
        Left(InvalidClusterWatchHeartbeatProblem(aId, badCoupled)))
      assert(watch.get.await(99.s) == Right(clusterState))
    }

    "FailedOver before heartbeat loss is rejected" in {
      scheduler.tick(1.s)
      assert(applyEvents(bId, ClusterFailedOver(aId, bId, failedAt) :: Nil) ==
        Left(ClusterWatchInactiveNodeProblem(bId, clusterState, 1.s,
          "event ClusterFailedOver(A --> B, JournalPosition(0,0)) --> FailedOver(passive A: http://A, active B: http://B, JournalPosition(0,0))")))
      assert(watch.isActive(aId).await(99.s).orThrow)
    }

    "FailedOver after heartbeat loss" in {
      scheduler.tick(20.s)
      assert(applyEvents(bId, ClusterFailedOver(aId, bId, failedAt) :: Nil) == Right(Completed))
      assert(watch.isActive(bId).await(99.s).orThrow)
    }

    "FailedOver to node without heartbeat" in {
      pending
      // FIXME ClusterWatch soll FailedOver ablehnen, wenn der zu aktivierende Knoten keinen aktuellen Herzschlag hat ?
      //  Oder FailedOver soll scheitern, wenn nicht eine absolute Mehrheit der ClusterWatch einen aktuellen Herzschlag
      //  für den zu aktivierenden Knoten hat.
    }

    "Coupled" in {
      assert(applyEvents(bId, ClusterCouplingPrepared(bId) :: Nil) == Right(Completed))
      assert(applyEvents(bId, ClusterCoupled(bId) :: Nil) == Right(Completed))
    }

    "SwitchedOver before heartbeat" in {
      scheduler.tick(1.s)
      assert(applyEvents(aId, ClusterSwitchedOver(aId) :: Nil) == Right(Completed))
      assert(watch.isActive(aId).await(99.s).orThrow)
    }

    "SwitchedOver after heartbeat loss" in {
      scheduler.tick(11.s)
      assert(applyEvents(aId, ClusterCouplingPrepared(aId) :: ClusterCoupled(aId) :: ClusterSwitchedOver(bId) :: Nil) == Right(Completed))
      assert(watch.isActive(bId).await(99.s).orThrow)
    }

    "SwitchedOver from still active node" in {
      assert(applyEvents(bId, ClusterCouplingPrepared(bId) :: ClusterCoupled(bId) :: Nil) == Right(Completed))
      assert(watch.isActive(bId).await(99.s).orThrow)
      pending // Not checked
      assert(applyEvents(bId, ClusterSwitchedOver(aId) :: Nil) ==
        Left(ClusterWatchInactiveNodeProblem(aId, clusterState, 0.s,
          "event ClusterSwitchedOver(A) --> SwitchedOver(active A: http://A, passive B: http://B)")))
    }

    "applyEvents after event loss" in {
      assert(watch.get.await(99.s) == Right(clusterState))
      assert(watch.get.await(99.s) == Right(ClusterState.Coupled(setting.copy(activeId = bId))))

      // We test the loss of a PassiveLost event, and then apply Coupled
      val lostEvent = ClusterPassiveLost(aId)
      val decoupled = ClusterState.PassiveLost(setting.copy(activeId = bId))
      assert(clusterState.applyEvent(NoKey <-: lostEvent) == Right(decoupled))

      val nextEvents = ClusterCouplingPrepared(bId) :: ClusterCoupled(bId) :: Nil
      val coupled = ClusterState.Coupled(setting.copy(activeId = bId))
      assert(decoupled.applyEvents(nextEvents.map(NoKey <-: _)) == Right(coupled))

      assert(watch.applyEvents(ClusterWatchEvents(bId, nextEvents, coupled)).await(99.s) == Right(Completed))
      assert(watch.get.await(99.s) == Right(coupled))
    }

    def applyEvents(from: NodeId, events: Seq[ClusterEvent]): Checked[Completed] = {
      val expectedClusterState = clusterState.applyEvents(events.map(NoKey <-: _)).orThrow
      val response = watch.applyEvents(ClusterWatchEvents(from, events, expectedClusterState)).await(99.s)
      for (_ <- response) {
        clusterState = expectedClusterState
      }
      assert(watch.get.await(99.s) == Right(clusterState))
      response
    }
  }
}
