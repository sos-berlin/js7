package js7.core.cluster.watch

import js7.base.generic.Completed
import js7.base.monixutils.MonixDeadline
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.common.message.ProblemCodeMessages
import js7.core.cluster.watch.ClusterWatch.*
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterNodeLostEvent, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.Coupled
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
final class ClusterWatchTest extends OurTestSuite
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

  private lazy val scheduler = TestScheduler()

  "Sequence of operations" - {
    var clusterState: ClusterState = ClusterState.NodesAppointed(setting)
    lazy val watch = new ClusterWatch(ControllerId("CONTROLLER"), () => MonixDeadline.now(scheduler))

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

    "Invalid Heartbeat is rejected" in {
      // bId != clusterState.activeId
      assert(watch.heartbeat(bId, clusterState).await(99.s) ==
        Left(InvalidClusterWatchHeartbeatProblem(bId, clusterState)))
    }

    "Heartbeat from wrong node is rejected" in {
      assert(watch.heartbeat(bId, ClusterState.Coupled(setting.copy(activeId = bId))).await(99.s) ==
        Left(ClusterWatchInactiveNodeProblem(bId, clusterState, 0.s,
          "heartbeat Coupled(passive A: http://A, active B: http://B)")))

      assert(watch.test.clusterState == clusterState)
    }

    "Heartbeat must not change active URI" in {
      // The inactive primary node should not send a heartbeat
      val badCoupled = ClusterState.Coupled(setting.copy(activeId = bId))
      assert(watch.heartbeat(aId, badCoupled).await(99.s) ==
        Left(InvalidClusterWatchHeartbeatProblem(aId, badCoupled)))
      assert(watch.test.clusterState == clusterState)
    }

    "FailedOver before heartbeat loss is rejected" in {
      scheduler.tick(1.s)
      assert(applyEvents(bId, ClusterFailedOver(aId, bId, failedAt) :: Nil) ==
        Left(ClusterWatchInactiveNodeProblem(bId, clusterState, 1.s,
          "event ClusterFailedOver(A --> B, JournalPosition(0,0)) --> " +
            "FailedOver(A --> B at JournalPosition(0,0))")))
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
      val events = Seq(
        ClusterCouplingPrepared(aId),
        ClusterCoupled(aId),
        ClusterSwitchedOver(bId))
      assert(applyEvents(aId, events) == Right(Completed))
      assert(watch.isActive(bId).await(99.s).orThrow)
    }

    "SwitchedOver from still active node" in {
      val events = Seq(
        ClusterCouplingPrepared(bId),
        ClusterCoupled(bId))
      assert(applyEvents(bId, events) == Right(Completed))
      assert(watch.isActive(bId).await(99.s).orThrow)

      pending // Not checked
      assert(applyEvents(bId, ClusterSwitchedOver(aId) :: Nil) ==
        Left(ClusterWatchInactiveNodeProblem(aId, clusterState, 0.s,
          "event ClusterSwitchedOver(A) --> SwitchedOver(active A: http://A, passive B: http://B)")))
    }

    "applyEvents after event loss" in {
      assert(watch.test.clusterState == clusterState)
      assert(watch.test.clusterState == ClusterState.Coupled(setting.copy(activeId = bId)))

      // We test the loss of a PassiveLost event, and then apply Coupled
      val lostEvent = ClusterPassiveLost(aId)
      val decoupled = ClusterState.PassiveLost(setting.copy(activeId = bId))
      assert(clusterState.applyEvent(NoKey <-: lostEvent) == Right(decoupled))

      val nextEvents = ClusterCouplingPrepared(bId) :: ClusterCoupled(bId) :: Nil
      val coupled = ClusterState.Coupled(setting.copy(activeId = bId))
      assert(decoupled.applyEvents(nextEvents.map(NoKey <-: _)) == Right(coupled))

      assert(watch.applyEvents(ClusterWatchEvents(bId, nextEvents, coupled)).await(99.s) == Right(Completed))
      assert(watch.test.clusterState == coupled)
    }

    // applyEvents may be called after missing events (due to unreachability).
    // So applyEvents is right, as long as it origins from the active node.
    //"applyEvents rejects mismatching reportedClusterState" in {
    //  val hasNodes = watch.test.clusterState.asInstanceOf[Coupled]
    //  import hasNodes.{activeId, passiveId}
    //
    //  // ClusterWatchInactiveNodeProblem
    //  locally {
    //    val event = ClusterFailedOver(passiveId, activatedId = activeId, failedAt)
    //    val expectedClusterState = FailedOver(hasNodes.setting, failedAt)
    //    val response = watch.applyEvents(ClusterWatchEvents(passiveId, Seq(event), expectedClusterState))
    //      .await(99.s)
    //    assert(response == Left(ClusterWatchInactiveNodeProblem(from = passiveId, hasNodes, 0.s,
    //      "event ClusterFailedOver(A --> B, JournalPosition(0,0)) --> " +
    //        "FailedOver(passive A: http://A, active B: http://B, JournalPosition(0,0))")))
    //  }
    //
    //  // Good event, bad expectedClusterState
    //  locally {
    //    val event = ClusterSwitchedOver(activatedId = passiveId)
    //    val badExpectedClusterState = FailedOver(hasNodes.setting, JournalPosition(999, 999))
    //    val response = watch.applyEvents(ClusterWatchEvents(passiveId, Seq(event), badExpectedClusterState))
    //      .await(99.s)
    //    assert(response == Left(ClusterWatchEventMismatchProblem(
    //      Seq(event), SwitchedOver(setting), badExpectedClusterState)))
    //  }
    //}

    def applyEvents(from: NodeId, events: Seq[ClusterEvent]): Checked[Completed] = {
      val expectedClusterState = clusterState.applyEvents(events.map(NoKey <-: _)).orThrow
      val response = watch.applyEvents(ClusterWatchEvents(from, events, expectedClusterState)).await(99.s)
      for (_ <- response) {
        clusterState = expectedClusterState
      }
      assert(watch.test.clusterState == clusterState)
      response
    }
  }

  "requireLostAck" - {
    "ClusterFailedOver" in {
      import setting.{activeId, passiveId}
      checkRequireLostAck(ClusterFailedOver(activeId, activatedId = passiveId, failedAt))
    }

    "ClusterPassiveLost" in {
      import setting.passiveId
      checkRequireLostAck(ClusterPassiveLost(passiveId))
    }

    def checkRequireLostAck(event: ClusterNodeLostEvent): Unit = {
      val coupled = Coupled(setting)
      val clusterState: ClusterState = coupled
      lazy val watch = new ClusterWatch(
        ControllerId("CONTROLLER"),
        () => MonixDeadline.now(scheduler),
        requireLostAck = true,
        initialClusterState = Some(coupled))
      import coupled.{activeId, passiveId}

      assert(watch.acknowledgeLostNode(activeId).await(99.s) == Left(NoClusterNodeLostProblem))
      assert(watch.acknowledgeLostNode(passiveId).await(99.s) == Left(NoClusterNodeLostProblem))

      scheduler.tick(setting.timing.heartbeatValidDuration)
      assert(watch.acknowledgeLostNode(activeId).await(99.s) == Left(NoClusterNodeLostProblem))
      assert(watch.acknowledgeLostNode(passiveId).await(99.s) == Left(NoClusterNodeLostProblem))

      val expectedClusterState = clusterState.applyEvent(NoKey <-: event).orThrow

      val response = watch
        .applyEvents(ClusterWatchEvents(passiveId, Seq(event), expectedClusterState))
        .await(99.s)
      assert(response == Left(ExplicitClusterNodeLostAckRequiredProblem))
      assert(watch.test.clusterState == clusterState)

      import event.lostNodeId
      val notLostNodeId = setting.other(lostNodeId)
      assert(watch.acknowledgeLostNode(notLostNodeId).await(99.s) ==
        Left(NoClusterNodeLostProblem))

      watch.acknowledgeLostNode(lostNodeId).await(99.s).orThrow
      watch
        .applyEvents(ClusterWatchEvents(notLostNodeId, Seq(event), expectedClusterState))
        .await(99.s)
        .orThrow
      assert(watch.test.clusterState == expectedClusterState)

      assert(watch.acknowledgeLostNode(activeId).await(99.s) == Left(NoClusterNodeLostProblem))
      assert(watch.acknowledgeLostNode(passiveId).await(99.s) == Left(NoClusterNodeLostProblem))
    }
  }
}
