package js7.cluster.watch

import js7.base.generic.Completed
import js7.base.log.CorrelId
import js7.base.monixutils.MonixDeadline.now
import js7.base.monixutils.MonixDeadline.syntax.DeadlineSchedule
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.cluster.watch.api.ClusterWatchProblems.{ClusterFailOverWhilePassiveLostProblem, ClusterNodeLossNotAcknowledgedProblem, ClusterWatchHeartbeatMismatchProblem, ClusterWatchInactiveNodeProblem, InvalidClusterWatchHeartbeatProblem, NoClusterNodeLostProblem, UntaughtClusterWatchProblem}
import js7.common.message.ProblemCodeMessages
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterNodeLostEvent, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, FailedOver, HasNodes, NodesAppointed, PassiveLost, PreparedToBeCoupled, SwitchedOver}
import js7.data.cluster.ClusterWatchMessage.RequestId
import js7.data.cluster.{ClusterEvent, ClusterSetting, ClusterState, ClusterTiming, ClusterWatchCheckEvent}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournalPosition}
import js7.data.node.NodeId
import monix.execution.Scheduler.Implicits.traced
import monix.execution.schedulers.TestScheduler
import scala.concurrent.duration.FiniteDuration

final class ClusterWatchTest extends OurTestSuite
{
  ProblemCodeMessages.initialize()

  private val correlId = CorrelId("TEST")
  private val aId = NodeId("A")
  private val bId = NodeId("B")
  private val timing = ClusterTiming(3.s, 10.s)
  private val setting = ClusterSetting(
    Map(
      NodeId("A") -> Uri("http://A"),
      NodeId("B") -> Uri("http://B")),
    activeId = NodeId("A"),
    timing)
  private val failedAt = JournalPosition(EventId(0), 0)
  private lazy val scheduler = TestScheduler()

  "ClusterWatch" - {
    def newClusterWatch(initialState: Option[HasNodes] = None) = {
      val w = new ClusterWatch(() => scheduler.now)
      for (s <- initialState) w.heartbeat(s.activeId, s).await(99.s).orThrow
      w
    }

    "Initial (untaught) state" in {
      val watch = newClusterWatch()
      val clusterState = Coupled(setting)
      val event = ClusterFailedOver(aId, bId, failedAt)
      val failedOver = clusterState.applyEvent(event).orThrow.asInstanceOf[FailedOver]
      assert(watch.handleMessage(ClusterWatchCheckEvent(RequestId(123), correlId, bId, event, failedOver)).await(99.s) ==
        Left(UntaughtClusterWatchProblem))

      assert(watch.heartbeat(aId, clusterState).await(99.s) == Right(Completed))
      scheduler.tick(timing.clusterWatchHeartbeatValidDuration)
      assert(watch.handleMessage(ClusterWatchCheckEvent(RequestId(123), correlId, bId, event, failedOver)).await(99.s) ==
        Right(Completed))
    }

    "Early heartbeat" in {
      val clusterState = Coupled(setting)
      val watch = newClusterWatch(Some(clusterState))
      scheduler.tick(1.s)
      assert(watch.heartbeat(aId, clusterState).await(99.s) == Right(Completed))
    }

    "Late heartbeat" in {
      val clusterState = Coupled(setting)
      val watch = newClusterWatch(Some(clusterState))

      scheduler.tick(11.s)
      assert(watch.heartbeat(aId, clusterState).await(99.s) == Right(Completed))

      scheduler.tick(timing.clusterWatchHeartbeatValidDuration + 1.s)
      assert(watch.heartbeat(aId, clusterState).await(99.s) == Right(Completed))
      assert(watch.isActive(aId).await(99.s).orThrow)
    }

    "Coupling" in {
      val clusterState = NodesAppointed(setting)
      val watch = newClusterWatch()

      val event1 = ClusterCouplingPrepared(aId)
      val clusterState1 = clusterState.applyEvent(event1).orThrow.asInstanceOf[PreparedToBeCoupled]
      assert(watch.handleMessage(ClusterWatchCheckEvent(RequestId(123), correlId, aId, event1, clusterState1)).await(99.s)
        == Right(Completed))

      val event2 = ClusterCoupled(aId)
      val clusterState2 = clusterState1.applyEvent(event2).orThrow.asInstanceOf[Coupled]
      assert(watch.handleMessage(ClusterWatchCheckEvent(RequestId(123), correlId, aId, event2, clusterState2)).await(99.s) ==
        Right(Completed))
      assert(watch.isActive(aId).await(99.s).orThrow)
      assert(watch.clusterState.await(99.s) == Right(clusterState2))
    }

    "Late heartbeat after coupled" in {
      val clusterState = Coupled(setting)
      val watch = newClusterWatch(Some(clusterState))
      scheduler.tick(timing.clusterWatchHeartbeatValidDuration + 1.s)

      assert(watch.heartbeat(aId, clusterState).await(99.s) == Right(Completed))
      assert(watch.isActive(aId).await(99.s).orThrow)
    }

    "Heartbeat with different ClusterState from same active node is rejected" in {
      val clusterState = Coupled(setting)
      val watch = newClusterWatch(Some(clusterState))

      val reportedClusterState = PassiveLost(setting)
      assert(watch.heartbeat(aId, reportedClusterState).await(99.s) ==
        Left(ClusterWatchHeartbeatMismatchProblem(clusterState, reportedClusterState)))
    }

    "Heartbeat from wrong node is rejected" in {
      val clusterState = Coupled(setting)
      val watch = newClusterWatch(Some(clusterState))

      assert(watch.heartbeat(bId, clusterState).await(99.s) ==
        Left(InvalidClusterWatchHeartbeatProblem(bId, clusterState)))

      locally {
        assert(watch.heartbeat(bId, Coupled(setting.copy(activeId = bId))).await(99.s) ==
          Left(ClusterWatchInactiveNodeProblem(bId, clusterState, 0.s,
            "heartbeat Coupled(passive A: http://A, active B: http://B)")))
      }

      assert(watch.clusterState.await(99.s) == Right(clusterState))
    }

    "Heartbeat must not change active URI" in {
      val clusterState = Coupled(setting)
      val watch = newClusterWatch(Some(clusterState))

      // The inactive primary node should not send a heartbeat
      val badCoupled = Coupled(setting.copy(activeId = bId))
      assert(watch.heartbeat(aId, badCoupled).await(99.s) ==
        Left(InvalidClusterWatchHeartbeatProblem(aId, badCoupled)))
      assert(watch.clusterState.await(99.s) == Right(clusterState))
    }

    "FailedOver before heartbeat loss is rejected" in {
      val clusterState = Coupled(setting)
      val watch = newClusterWatch(Some(clusterState))
      val duration = timing.clusterWatchHeartbeatValidDuration - 1.ms
      scheduler.tick(duration)

      val event = ClusterFailedOver(aId, bId, failedAt)
      val clusterWatch2 = clusterState.applyEvent(event)
        .orThrow.asInstanceOf[FailedOver]
      assert(watch.handleMessage(ClusterWatchCheckEvent(RequestId(123), correlId, bId, event, clusterWatch2)).await(99.s) ==
        Left(ClusterWatchInactiveNodeProblem(bId, clusterState, duration,
          "ClusterFailedOver(A --> B, JournalPosition(0,0)) --> FailedOver(A --> B at JournalPosition(0,0))")))

      assert(watch.isActive(aId).await(99.s).orThrow)
    }

    "FailedOver but concurrent PassiveLost" in {
      val clusterState = Coupled(setting)
      val watch = newClusterWatch(Some(clusterState))
      scheduler.tick(timing.clusterWatchHeartbeatValidDuration - 1.ms)
      val failedOverEvent = ClusterFailedOver(aId, bId, failedAt)
      val failedOver = clusterState.applyEvent(failedOverEvent)
        .orThrow.asInstanceOf[FailedOver]

      val passiveLostEvent = ClusterPassiveLost(bId)
      val passiveLost = clusterState.applyEvent(passiveLostEvent)
        .orThrow.asInstanceOf[PassiveLost]
      assert(watch.handleMessage(ClusterWatchCheckEvent(RequestId(123), correlId, aId, passiveLostEvent, passiveLost)).await(99.s) ==
        Right(Completed))

      assert(watch.handleMessage(ClusterWatchCheckEvent(RequestId(123), correlId, bId, failedOverEvent, failedOver)).await(99.s)
        == Left(ClusterFailOverWhilePassiveLostProblem))

      assert(watch.isActive(aId).await(99.s).orThrow)
    }

    "FailedOver after heartbeat loss" in {
      val clusterState = Coupled(setting)
      val watch = newClusterWatch(Some(clusterState))
      scheduler.tick(timing.clusterWatchHeartbeatValidDuration)

      val event = ClusterFailedOver(aId, bId, failedAt)
      val clusterWatch2 = clusterState.applyEvent(event)
        .orThrow.asInstanceOf[FailedOver]
      assert(watch.handleMessage(ClusterWatchCheckEvent(RequestId(123), correlId, bId, event, clusterWatch2)).await(99.s) ==
        Right(Completed))
      assert(watch.isActive(bId).await(99.s).orThrow)
    }

    "Coupled" in {
      val clusterState = NodesAppointed(setting)
      implicit val watch = newClusterWatch()

      val couplingPreparedEvent = ClusterCouplingPrepared(aId)
      val couplingPrepared = clusterState.applyEvent(couplingPreparedEvent)
        .orThrow.asInstanceOf[PreparedToBeCoupled]
      assert(applyEvent(clusterState, aId, couplingPreparedEvent, couplingPrepared).isRight)

      val coupledEvent = ClusterCoupled(aId)
      val coupled = couplingPrepared.applyEvent(coupledEvent).orThrow.asInstanceOf[Coupled]
      assert(applyEvent(couplingPrepared, aId, coupledEvent, coupled).isRight)
    }

    "SwitchedOver before heartbeat" in {
      testSwitchOver(1.s)
    }

    "SwitchedOver after heartbeat loss" in {
      testSwitchOver(timing.clusterWatchHeartbeatValidDuration + 1.s)
    }

    def testSwitchOver(duration: FiniteDuration): Unit = {
      val clusterState = Coupled(setting)
      implicit val watch = newClusterWatch(Some(clusterState))
      scheduler.tick(duration)
      val event = ClusterSwitchedOver(bId)
      val switchedOver = SwitchedOver(setting.copy(activeId = bId))
      assert(applyEvent(clusterState, aId, event, switchedOver).isRight)
      assert(applyEvent(clusterState, bId, event, switchedOver).isRight)
      assert(watch.isActive(bId).await(99.s).orThrow)
    }

    "applyEvents after event loss" in {
      val clusterState = Coupled(setting)
      val watch = newClusterWatch(Some(clusterState))

      // We test the loss of a PassiveLost event, and then apply Coupled
      val passiveLostEvent = ClusterPassiveLost(bId)
      val passiveLost = PassiveLost(setting.copy(activeId = aId))
      val prepared = PreparedToBeCoupled(setting.copy(activeId = aId))
      assert(clusterState.applyEvent(passiveLostEvent) == Right(passiveLost))
      assert(watch.handleMessage(ClusterWatchCheckEvent(RequestId(123), correlId, aId, passiveLostEvent, prepared)).await(99.s) ==
        Right(Completed))

      locally {
        val nextEvent1 = ClusterCouplingPrepared(aId)
        assert(passiveLost.applyEvent(nextEvent1) == Right(prepared))

        assert(watch.handleMessage(ClusterWatchCheckEvent(RequestId(123), correlId, aId, nextEvent1, prepared)).await(99.s) ==
          Right(Completed))
        assert(watch.clusterState.await(99.s) == Right(prepared))
      }

      locally {
        val nextEvent2 = ClusterCoupled(aId)
        val coupled = Coupled(setting.copy(activeId = aId))
        assert(prepared.applyEvent(nextEvent2) == Right(coupled))

        assert(watch.handleMessage(ClusterWatchCheckEvent(RequestId(123), correlId, aId, nextEvent2, coupled)).await(99.s) ==
          Right(Completed))
        assert(watch.clusterState.await(99.s) == Right(coupled))
      }
    }

    def applyEvent(
      clusterState: ClusterState,
      from: NodeId,
      event: ClusterEvent,
      expectedClusterState: HasNodes)
      (implicit watch: ClusterWatch)
    : Checked[Completed] = {
      assert(expectedClusterState == clusterState.applyEvent(event).orThrow)
      val response = watch.handleMessage(ClusterWatchCheckEvent(RequestId(123), correlId, from, event, expectedClusterState))
        .await(99.s)
      assert(watch.clusterState.await(99.s) == Right(expectedClusterState))
      response
    }
  }

  "State.canBeTheActiveNode" in {
    assert(ClusterWatch.State(Coupled(setting), now - setting.timing.heartbeat, None).canBeTheActiveNode(aId))
  }

  "requireLostAck" - {
    "ClusterFailedOver" in {
      import setting.{activeId, passiveId}
      checkRequireLostAck(from = passiveId, ClusterFailedOver(activeId, activatedId = passiveId, failedAt))
    }

    "ClusterPassiveLost" in {
      import setting.{activeId, passiveId}
      checkRequireLostAck(from = activeId, ClusterPassiveLost(passiveId))
    }

    def checkRequireLostAck(from: NodeId, event: ClusterNodeLostEvent): Unit = {
      val coupled = Coupled(setting)
      import coupled.{activeId, passiveId}

      lazy val watch = new ClusterWatch(
        () => scheduler.now,
        requireLostAck = true)

      // Initialize ClusterWatch
      watch.heartbeat(activeId, coupled).await(99.s).orThrow

      assert(watch.acknowledgeLostNode(activeId).await(99.s) == Left(NoClusterNodeLostProblem))
      assert(watch.acknowledgeLostNode(passiveId).await(99.s) == Left(NoClusterNodeLostProblem))

      scheduler.tick(setting.timing.clusterWatchHeartbeatValidDuration)
      assert(watch.acknowledgeLostNode(activeId).await(99.s) == Left(NoClusterNodeLostProblem))
      assert(watch.acknowledgeLostNode(passiveId).await(99.s) == Left(NoClusterNodeLostProblem))

      val expectedClusterState = coupled.applyEvent(NoKey <-: event).orThrow.asInstanceOf[HasNodes]

      // Event is rejected because Node loss has not yet been acknowledged
      val response = watch
        .handleMessage(ClusterWatchCheckEvent(RequestId(123), correlId, from, event, expectedClusterState))
        .await(99.s)
      assert(response == Left(ClusterNodeLossNotAcknowledgedProblem(event)))
      assert(watch.currentClusterState == Some(coupled))

      // Try to acknowledge a loss of the not lost Node
      import event.lostNodeId
      val notLostNodeId = setting.other(lostNodeId)
      assert(watch.acknowledgeLostNode(notLostNodeId).await(99.s) ==
        Left(NoClusterNodeLostProblem))

      // Acknowledge the loss of the Node
      watch.acknowledgeLostNode(lostNodeId).await(99.s).orThrow
      //scheduler.tick(setting.timing.clusterWatchHeartbeatValidDuration)
      watch
        .handleMessage(ClusterWatchCheckEvent(RequestId(123), correlId, notLostNodeId, event, expectedClusterState))
        .await(99.s)
        .orThrow
      assert(watch.currentClusterState == Some(expectedClusterState))

      assert(watch.acknowledgeLostNode(activeId).await(99.s) == Left(NoClusterNodeLostProblem))
      assert(watch.acknowledgeLostNode(passiveId).await(99.s) == Left(NoClusterNodeLostProblem))
    }
  }
}
