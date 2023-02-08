package js7.cluster.watch

import js7.base.generic.Completed
import js7.base.log.CorrelId
import js7.base.monixutils.MonixDeadline.syntax.DeadlineSchedule
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.cluster.watch.api.ClusterWatchProblems.{ClusterFailOverWhilePassiveLostProblem, ClusterNodeLossNotConfirmedProblem, ClusterWatchInactiveNodeProblem, NoClusterNodeLostProblem, UntaughtClusterWatchProblem}
import js7.common.message.ProblemCodeMessages
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterNodeLostEvent, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, FailedOver, HasNodes, NodesAppointed, PassiveLost, PreparedToBeCoupled, SwitchedOver}
import js7.data.cluster.ClusterWatchRequest.RequestId
import js7.data.cluster.{ClusterEvent, ClusterSetting, ClusterState, ClusterTiming, ClusterWatchCheckEvent, ClusterWatchCheckState, InvalidClusterWatchHeartbeatProblem}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournalPosition}
import js7.data.node.NodeId
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
    def newClusterWatch(initialState: Option[HasNodes] = None): ClusterWatch = {
      implicit val watch = new ClusterWatch(() => scheduler.now)
      for (clusterState <- initialState) {
        heartbeat(clusterState.activeId, clusterState).orThrow
      }
      watch
    }

    "Initial (untaught) state" in {
      implicit val watch = newClusterWatch()
      val clusterState = Coupled(setting)
      val event = ClusterFailedOver(aId, bId, failedAt)
      val failedOver = clusterState.applyEvent(event).orThrow.asInstanceOf[FailedOver]
      assert(watch.processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, bId, event, failedOver))
         == Left(UntaughtClusterWatchProblem))

      assert(heartbeat(aId, clusterState) == Right(Completed))

      scheduler.tick(timing.clusterWatchHeartbeatValidDuration)
      assert(watch.processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, bId, event, failedOver))
        == Right(Completed))
    }

    "Early heartbeat" in {
      val clusterState = Coupled(setting)
      implicit val watch = newClusterWatch(Some(clusterState))
      scheduler.tick(1.s)
      assert(heartbeat(aId, clusterState) == Right(Completed))
    }

    "Late heartbeat" in {
      val clusterState = Coupled(setting)
      implicit val watch = newClusterWatch(Some(clusterState))

      scheduler.tick(11.s)
      assert(heartbeat(aId, clusterState) == Right(Completed))

      scheduler.tick(timing.clusterWatchHeartbeatValidDuration + 1.s)
      assert(heartbeat(aId, clusterState) == Right(Completed))
      assert(watch.isActive(aId).orThrow)
    }

    "Coupling" in {
      val clusterState = NodesAppointed(setting)
      val watch = newClusterWatch()

      val event1 = ClusterCouplingPrepared(aId)
      val clusterState1 = clusterState.applyEvent(event1).orThrow.asInstanceOf[PreparedToBeCoupled]
      assert(watch
        .processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, aId, event1, clusterState1))
        == Right(Completed))

      val event2 = ClusterCoupled(aId)
      val clusterState2 = clusterState1.applyEvent(event2).orThrow.asInstanceOf[Coupled]
      assert(watch
        .processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, aId, event2, clusterState2))
        == Right(Completed))
      assert(watch.isActive(aId).orThrow)
      assert(watch.unsafeClusterState() == Right(clusterState2))
    }

    "Late heartbeat after coupled" in {
      val clusterState = Coupled(setting)
      implicit val watch = newClusterWatch(Some(clusterState))
      scheduler.tick(timing.clusterWatchHeartbeatValidDuration + 1.s)

      assert(heartbeat(aId, clusterState) == Right(Completed))
      assert(watch.isActive(aId).orThrow)
    }

    "Heartbeat with different ClusterState from same active node is accepted" in {
      val clusterState = Coupled(setting)
      implicit val watch = newClusterWatch(Some(clusterState))

      val reportedClusterState = PassiveLost(setting)
      assert(heartbeat(aId, reportedClusterState).isRight)
    }

    "Heartbeat from wrong node is rejected" in {
      val clusterState = Coupled(setting)
      implicit val watch = newClusterWatch(Some(clusterState))

      assert(heartbeat(bId, clusterState) ==
        Left(InvalidClusterWatchHeartbeatProblem(bId, clusterState)))

      locally {
        assert(heartbeat(bId, Coupled(setting.copy(activeId = bId))) ==
          Left(ClusterWatchInactiveNodeProblem(bId, clusterState, 0.s,
            "heartbeat --> Coupled(passive A: http://A, active B: http://B)")))
      }

      assert(watch.unsafeClusterState() == Right(clusterState))
    }

    "Heartbeat must not change active URI" in {
      val clusterState = Coupled(setting)
      implicit val watch = newClusterWatch(Some(clusterState))

      // The inactive primary node should not send a heartbeat
      val badCoupled = Coupled(setting.copy(activeId = bId))
      assert(heartbeat(aId, badCoupled) ==
        Left(InvalidClusterWatchHeartbeatProblem(aId, badCoupled)))
      assert(watch.unsafeClusterState() == Right(clusterState))
    }

    "FailedOver before heartbeat loss is rejected" in {
      val clusterState = Coupled(setting)
      val watch = newClusterWatch(Some(clusterState))
      val duration = timing.clusterWatchHeartbeatValidDuration - 1.ms
      scheduler.tick(duration)

      val event = ClusterFailedOver(aId, bId, failedAt)
      val clusterWatch2 = clusterState.applyEvent(event)
        .orThrow.asInstanceOf[FailedOver]
      assert(watch.processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, bId, event, clusterWatch2))
        == Left(ClusterWatchInactiveNodeProblem(bId, clusterState, duration,
          "ClusterFailedOver(A --> B, JournalPosition(0,0)) --> FailedOver(A --> B at JournalPosition(0,0))")))

      assert(watch.isActive(aId).orThrow)
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
      assert(watch.processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, aId, passiveLostEvent, passiveLost))
        == Right(Completed))

      assert(watch.processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, bId, failedOverEvent, failedOver))
        == Left(ClusterFailOverWhilePassiveLostProblem))

      assert(watch.isActive(aId).orThrow)
    }

    "FailedOver after heartbeat loss" in {
      val clusterState = Coupled(setting)
      val watch = newClusterWatch(Some(clusterState))
      scheduler.tick(timing.clusterWatchHeartbeatValidDuration)

      val event = ClusterFailedOver(aId, bId, failedAt)
      val clusterWatch2 = clusterState.applyEvent(event)
        .orThrow.asInstanceOf[FailedOver]
      assert(watch.processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, bId, event, clusterWatch2))
        == Right(Completed))
      assert(watch.isActive(bId).orThrow)
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
      assert(watch.isActive(bId).orThrow)

      // Duplicate
      assert(applyEvent(clusterState, bId, event, switchedOver).isRight)
      assert(watch.isActive(bId).orThrow)
    }

    "applyEvent after event loss" in {
      val clusterState = Coupled(setting)
      val watch = newClusterWatch(Some(clusterState))

      // We test the loss of a PassiveLost event, and then apply Coupled
      val passiveLostEvent = ClusterPassiveLost(bId)
      val passiveLost = PassiveLost(setting.copy(activeId = aId))
      val prepared = PreparedToBeCoupled(setting.copy(activeId = aId))
      assert(clusterState.applyEvent(passiveLostEvent) == Right(passiveLost))
      assert(watch.processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, aId, passiveLostEvent, prepared))
        == Right(Completed))

      locally {
        val nextEvent1 = ClusterCouplingPrepared(aId)
        assert(passiveLost.applyEvent(nextEvent1) == Right(prepared))

        assert(watch.processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, aId, nextEvent1, prepared))
          == Right(Completed))
        assert(watch.unsafeClusterState() == Right(prepared))
      }

      locally {
        val nextEvent2 = ClusterCoupled(aId)
        val coupled = Coupled(setting.copy(activeId = aId))
        assert(prepared.applyEvent(nextEvent2) == Right(coupled))

        assert(watch.processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, aId, nextEvent2, coupled))
          == Right(Completed))
        assert(watch.unsafeClusterState() == Right(coupled))
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
      val response = watch.processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, from, event, expectedClusterState))
      assert(watch.unsafeClusterState() == Right(expectedClusterState))
      response
    }

    def heartbeat(from: NodeId, clusterState: HasNodes)(implicit watch: ClusterWatch)
    : Checked[Completed] =
      watch.processRequest(ClusterWatchCheckState(RequestId(123), correlId, from, clusterState))
  }

  "requireManualNodeLossConfirmation" - {
    "ClusterFailedOver" in {
      import setting.{activeId, passiveId}
      checkRequireNodeLossConfirm(from = passiveId, ClusterFailedOver(activeId, activatedId = passiveId, failedAt))
    }

    "ClusterPassiveLost" in {
      import setting.{activeId, passiveId}
      checkRequireNodeLossConfirm(from = activeId, ClusterPassiveLost(passiveId))
    }

    def checkRequireNodeLossConfirm(from: NodeId, event: ClusterNodeLostEvent): Unit = {
      val coupled = Coupled(setting)
      import coupled.{activeId, passiveId}

      lazy val watch = new ClusterWatch(
        () => scheduler.now,
        requireManualNodeLossConfirmation = true)

      // Initialize ClusterWatch
      watch.processRequest(ClusterWatchCheckState(RequestId(123), correlId, activeId, coupled))
        .orThrow

      assert(watch.confirmNodeLoss(activeId) == Left(NoClusterNodeLostProblem))
      assert(watch.confirmNodeLoss(passiveId) == Left(NoClusterNodeLostProblem))

      scheduler.tick(setting.timing.clusterWatchHeartbeatValidDuration)
      assert(watch.confirmNodeLoss(activeId) == Left(NoClusterNodeLostProblem))
      assert(watch.confirmNodeLoss(passiveId) == Left(NoClusterNodeLostProblem))

      val expectedClusterState = coupled.applyEvent(NoKey <-: event).orThrow.asInstanceOf[HasNodes]

      // Event is rejected because Node loss has not yet been confirmed
      val response = watch
        .processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, from, event, expectedClusterState))
      assert(response == Left(ClusterNodeLossNotConfirmedProblem(event)))
      assert(watch.unsafeClusterState() == Right(coupled))

      // Try to confirm a loss of the not lost Node
      import event.lostNodeId
      val notLostNodeId = setting.other(lostNodeId)
      assert(watch.confirmNodeLoss(notLostNodeId) ==
        Left(NoClusterNodeLostProblem))

      // Confirm the loss of the Node
      watch.confirmNodeLoss(lostNodeId).orThrow
      //scheduler.tick(setting.timing.clusterWatchHeartbeatValidDuration)
      watch
        .processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, notLostNodeId, event, expectedClusterState))

        .orThrow
      assert(watch.unsafeClusterState() == Right(expectedClusterState))

      assert(watch.confirmNodeLoss(activeId) == Left(NoClusterNodeLostProblem))
      assert(watch.confirmNodeLoss(passiveId) == Left(NoClusterNodeLostProblem))
    }
  }
}
