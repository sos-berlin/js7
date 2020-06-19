package js7.core.cluster

import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.common.scalautil.MonixUtils.syntax._
import js7.core.cluster.ClusterWatch._
import js7.core.message.ProblemCodeMessages
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.{ClusterEvent, ClusterNodeId, ClusterState}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournalPosition}
import monix.execution.Scheduler.Implicits.global
import monix.execution.schedulers.TestScheduler
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterWatchTest extends AnyFreeSpec
{
  ProblemCodeMessages.initialize()

  private val aId = ClusterNodeId("A")
  private val bId = ClusterNodeId("B")
  private val aUri = Uri("http://A")
  private val bUri = Uri("http://B")
  private val idToUri = Map(aId -> aUri, bId -> bUri)
  private val failedAt = JournalPosition(EventId(0), 0)

  "ClusterWatch" - {
    lazy val scheduler = TestScheduler()
    var clusterState: ClusterState = ClusterState.NodesAppointed(idToUri, aId)
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

    "Heartbeat with different ClusterState from same active node is accepted" in {
      val reportedClusterState = ClusterState.PassiveLost(idToUri, activeId = aId)
      assert(watch.heartbeat(aId, reportedClusterState).await(99.s) == Right(Completed))
      assert(watch.get.await(99.s) == Right(reportedClusterState))

      // Restore ClusterState for following tests
      assert(watch.heartbeat(aId, clusterState).await(99.s) == Right(Completed))
      assert(watch.get.await(99.s) == Right(clusterState))
    }

    "Heartbeat from wrong node is rejected" in {
      assert(watch.heartbeat(bId, clusterState).await(99.s) ==
        Left(ClusterWatchHeartbeatFromInactiveNodeProblem(bId, clusterState)))

      locally {
        assert(watch.heartbeat(bId, ClusterState.Coupled(idToUri, activeId = bId)).await(99.s) ==
          Left(ClusterWatchHeartbeatFromInactiveNodeProblem(bId, clusterState)))
      }

      assert(watch.get.await(99.s) == Right(clusterState))
    }

    "Heartbeat must not change active URI" in {
      // The inactive primary node should not send a heartbeat
      val badCoupled = ClusterState.Coupled(idToUri, activeId = bId)
      assert(watch.heartbeat(aId, badCoupled).await(99.s) ==
        Left(InvalidClusterWatchHeartbeatProblem(aId, badCoupled)))
      assert(watch.get.await(99.s) == Right(clusterState))
    }

    "FailedOver before heartbeat loss is rejected" in {
      scheduler.tick(1.s)
      assert(applyEvents(bId, ClusterFailedOver(aId, bId, failedAt) :: Nil) ==
        Left(ClusterWatchHeartbeatFromInactiveNodeProblem(bId, clusterState)))
      assert(watch.isActive(aId).await(99.s).orThrow)
    }

    "FailedOver after heartbeat loss" in {
      scheduler.tick(11.s)
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
      assert(applyEvents(bId, ClusterSwitchedOver(aId) :: Nil) == Right(Completed))
      assert(watch.isActive(aId).await(99.s).orThrow)
    }

    "SwitchedOver after heartbeat loss" in {
      scheduler.tick(11.s)
      assert(applyEvents(aId, ClusterCouplingPrepared(aId) :: ClusterCoupled(aId) :: ClusterSwitchedOver(bId) :: Nil) == Right(Completed))
      assert(watch.isActive(bId).await(99.s).orThrow)
    }

    "SwitchedOver from inactive node" in {
      assert(applyEvents(bId, ClusterCouplingPrepared(bId) :: ClusterCoupled(bId) :: Nil) == Right(Completed))
      assert(watch.isActive(bId).await(99.s).orThrow)
      assert(applyEvents(aId, ClusterSwitchedOver(aId) :: Nil) ==
        Left(ClusterWatchHeartbeatFromInactiveNodeProblem(aId, clusterState)))
    }

    "applyEvents after event loss" in {
      assert(watch.get.await(99.s) == Right(clusterState))
      assert(watch.get.await(99.s) == Right(ClusterState.Coupled(idToUri, activeId = bId)))

      // We test the loss of a PassiveLost event, and then apply Coupled
      val lostEvent = ClusterPassiveLost(aId)
      val decoupled = ClusterState.PassiveLost(idToUri, activeId = bId)
      assert(clusterState.applyEvent(NoKey <-: lostEvent) == Right(decoupled))

      val nextEvents = ClusterCouplingPrepared(bId) :: ClusterCoupled(bId) :: Nil
      val coupled = ClusterState.Coupled(idToUri, activeId = bId)
      assert(decoupled.applyEvents(nextEvents.map(NoKey <-: _)) == Right(coupled))

      assert(watch.applyEvents(bId, nextEvents, coupled).await(99.s) == Right(Completed))
      assert(watch.get.await(99.s) == Right(coupled))
    }

    def applyEvents(from: ClusterNodeId, events: Seq[ClusterEvent]): Checked[Completed] = {
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
