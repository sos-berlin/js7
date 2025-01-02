package js7.cluster.watch

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.effect.unsafe.IORuntime
import js7.base.log.CorrelId
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.cluster.watch.ClusterWatch.Confirmed
import js7.common.message.ProblemCodeMessages
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, FailedOver, HasNodes, NodesAppointed, PassiveLost, PreparedToBeCoupled, SwitchedOver}
import js7.data.cluster.ClusterWatchProblems.{ClusterFailOverWhilePassiveLostProblem, ClusterNodeIsNotLostProblem, ClusterNodeLossNotConfirmedProblem, ClusterWatchInactiveNodeProblem, UntaughtClusterWatchProblem}
import js7.data.cluster.ClusterWatchRequest.RequestId
import js7.data.cluster.{ClusterEvent, ClusterSetting, ClusterState, ClusterTiming, ClusterWatchCheckEvent, ClusterWatchCheckState, InvalidClusterWatchHeartbeatProblem}
import js7.data.event.{EventId, JournalPosition}
import js7.data.node.NodeId
import org.scalatest.Assertion
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

final class ClusterWatchTest extends OurAsyncTestSuite:

  ProblemCodeMessages.initialize()

  private given IORuntime = ioRuntime

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

  "ClusterWatch" - {
    def newClusterWatch(initialState: Option[HasNodes] = None): IO[ClusterWatch] =
      implicit val watch = new ClusterWatch
      for
        _ <- initialState.fold(IO.unit): clusterState =>
          heartbeat(clusterState.activeId, clusterState).map(_.orThrow)
      yield
        watch

    "Initial (untaught) state, active node heartbeats, after delay passive fails-over" in:
      TestControl.executeEmbed:
        newClusterWatch().flatMap(implicit watch =>
          val clusterState = Coupled(setting)
          val event = ClusterFailedOver(aId, bId, failedAt)
          val failedOver = clusterState.applyEvent(event).orThrow.asInstanceOf[FailedOver]
          for
            confirmed <- watch.processRequest:
              ClusterWatchCheckEvent(RequestId(123), correlId, bId, event, failedOver)
            _ = assert(confirmed == Left(ClusterNodeLossNotConfirmedProblem(bId, event)))
            confirmed <- heartbeat(aId, clusterState)
            _ = assert(confirmed == Right(Confirmed()))
            _ <- IO.sleep(timing.clusterWatchHeartbeatValidDuration)
            confirmed <- watch.processRequest:
              ClusterWatchCheckEvent(RequestId(123), correlId, bId, event, failedOver)
          yield
            assert(confirmed == Right(Confirmed())))

    "Early heartbeat" in:
      TestControl.executeEmbed:
        val clusterState = Coupled(setting)
        newClusterWatch(Some(clusterState)).flatMap(implicit watch =>
          for
            _ <- IO.sleep(1.s)
            confirmed <- heartbeat(aId, clusterState)
          yield
            assert(confirmed == Right(Confirmed())))

    "Late heartbeat" in:
      TestControl.executeEmbed:
        val clusterState = Coupled(setting)
        newClusterWatch(Some(clusterState)).flatMap(implicit watch =>
          for
            _ <- IO.sleep(11.s)
            confirmed <- heartbeat(aId, clusterState)
            _ = assert(confirmed == Right(Confirmed()))
            _ <- IO.sleep(timing.clusterWatchHeartbeatValidDuration + 1.s)
            confirmed <- heartbeat(aId, clusterState)
          yield
            assert(confirmed == Right(Confirmed()))
            assert(watch.isActive(aId).orThrow))

    "Coupling" in:
      TestControl.executeEmbed:
        val clusterState = NodesAppointed(setting)
        newClusterWatch().flatMap { implicit watch =>
          val event1 = ClusterCouplingPrepared(aId)
          val clusterState1 = clusterState.applyEvent(event1).orThrow.asInstanceOf[PreparedToBeCoupled]
          for
            confirmed <- watch.processRequest:
              ClusterWatchCheckEvent(RequestId(123), correlId, aId, event1, clusterState1)
            _ = assert(confirmed == Right(Confirmed()))

            event2 = ClusterCoupled(aId)
            clusterState2 = clusterState1.applyEvent(event2).orThrow.asInstanceOf[Coupled]
            confirmed <- watch.processRequest:
              ClusterWatchCheckEvent(RequestId(123), correlId, aId, event2, clusterState2)
          yield
            assert(confirmed == Right(Confirmed()))
            assert(watch.isActive(aId).orThrow)
            assert(watch.clusterState() == Right(clusterState2))
        }

    "Late heartbeat after coupled" in:
      TestControl.executeEmbed:
        val clusterState = Coupled(setting)
        newClusterWatch(Some(clusterState)).flatMap(implicit watch =>
          for
            _ <- IO.sleep(timing.clusterWatchHeartbeatValidDuration + 1.s)
            confirmed <- heartbeat(aId, clusterState)
          yield
            assert(confirmed == Right(Confirmed()))
            assert(watch.isActive(aId).orThrow))

    "Heartbeat with different ClusterState from same active node is accepted" in:
      val clusterState = Coupled(setting)
      val reportedClusterState = PassiveLost(setting)
      TestControl.executeEmbed:
        newClusterWatch(Some(clusterState)).flatMap(implicit watch =>
          for
            confirmed <- heartbeat(aId, reportedClusterState)
          yield
            assert(confirmed.isRight))

    "Heartbeat from wrong node is rejected" in:
      val clusterState = Coupled(setting)
      TestControl.executeEmbed:
        newClusterWatch(Some(clusterState)).flatMap(implicit watch =>
          for
            confirmed <- heartbeat(bId, clusterState)
            _ = assert(confirmed == Left(InvalidClusterWatchHeartbeatProblem(bId, clusterState)))
            confirmed <- heartbeat(bId, Coupled(setting.copy(activeId = bId)))
          yield
            assert(confirmed == Left(
              ClusterWatchInactiveNodeProblem(bId, clusterState, 0.s,
                "Coupled(passive A http://A, active B http://B)")))
            assert(watch.clusterState() == Right(clusterState)))

    "Heartbeat must not change active URI" in:
      val clusterState = Coupled(setting)
      val badCoupled = Coupled(setting.copy(activeId = bId))
      TestControl.executeEmbed:
        newClusterWatch(Some(clusterState)).flatMap(implicit watch =>
          for
            confirmed <- heartbeat(aId, badCoupled)
            // The inactive primary node should not send a heartbeat
          yield
            assert(confirmed == Left(InvalidClusterWatchHeartbeatProblem(aId, badCoupled)))
            assert(watch.clusterState() == Right(clusterState)))

    "FailedOver before heartbeat loss is rejected" in:
      val clusterState = Coupled(setting)
      val duration = timing.clusterWatchHeartbeatValidDuration - 1.ms
      TestControl.executeEmbed:
        newClusterWatch(Some(clusterState)).flatMap { implicit watch =>
          for
            _ <- IO.sleep(duration)
            event = ClusterFailedOver(aId, bId, failedAt)
            clusterWatch2 = clusterState.applyEvent(event).orThrow.asInstanceOf[FailedOver]
            confirmed <- watch.processRequest:
              ClusterWatchCheckEvent(RequestId(123), correlId, bId, event, clusterWatch2)
          yield
            assert(confirmed == Left(ClusterWatchInactiveNodeProblem(bId, clusterState, duration,
              "ClusterFailedOver(A --> B, JournalPosition(0,0)) --> FailedOver(A --> B at JournalPosition(0,0))")))
            assert(watch.isActive(aId).orThrow)
        }

    "FailedOver but concurrent PassiveLost" in:
      val clusterState = Coupled(setting)
      TestControl.executeEmbed:
        newClusterWatch(Some(clusterState)).flatMap { implicit watch =>
          for
            _ <- IO.sleep(timing.clusterWatchHeartbeatValidDuration - 1.ms)
            failedOverEvent = ClusterFailedOver(aId, bId, failedAt)
            failedOver = clusterState.applyEvent(failedOverEvent).orThrow.asInstanceOf[FailedOver]
            passiveLostEvent = ClusterPassiveLost(bId)
            passiveLost = clusterState.applyEvent(passiveLostEvent)
              .orThrow.asInstanceOf[PassiveLost]
            confirmed <- watch.processRequest(
              ClusterWatchCheckEvent(RequestId(123), correlId, aId, passiveLostEvent, passiveLost))
            _ = assert(confirmed == Right(Confirmed()))
            confirmed <- watch
              .processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, bId, failedOverEvent, failedOver))
          yield
            assert(confirmed == Left(ClusterFailOverWhilePassiveLostProblem))
            assert(watch.isActive(aId).orThrow)
        }

    "FailedOver after heartbeat loss" in:
      val clusterState = Coupled(setting)
      TestControl.executeEmbed:
        newClusterWatch(Some(clusterState)).flatMap { implicit watch =>
          for
            _ <- IO.sleep(timing.clusterWatchHeartbeatValidDuration)
            event = ClusterFailedOver(aId, bId, failedAt)
            clusterWatch2 = clusterState.applyEvent(event).orThrow.asInstanceOf[FailedOver]
            confirmed <- watch.processRequest(
              ClusterWatchCheckEvent(RequestId(123), correlId, bId, event, clusterWatch2))
          yield
            assert(confirmed == Right(Confirmed()))
            assert(watch.isActive(bId).orThrow)
        }

    "Coupled" in:
      val clusterState = NodesAppointed(setting)
      TestControl.executeEmbed:
        newClusterWatch(Some(clusterState)).flatMap { implicit watch =>
          val couplingPreparedEvent = ClusterCouplingPrepared(aId)
          val couplingPrepared = clusterState.applyEvent(couplingPreparedEvent)
            .orThrow.asInstanceOf[PreparedToBeCoupled]
          for
            confirmed <- applyEvent(clusterState, aId, couplingPreparedEvent, couplingPrepared)
            _ = assert(confirmed == Right(Confirmed()))
            coupledEvent = ClusterCoupled(aId)
            coupled = couplingPrepared.applyEvent(coupledEvent).orThrow.asInstanceOf[Coupled]
            confirmed <- applyEvent(couplingPrepared, aId, coupledEvent, coupled)
          yield
            assert(confirmed == Right(Confirmed()))
        }

    "SwitchedOver before heartbeat" in:
      testSwitchOver(1.s)

    "SwitchedOver after heartbeat loss" in:
      testSwitchOver(timing.clusterWatchHeartbeatValidDuration + 1.s)

    def testSwitchOver(duration: FiniteDuration): IO[Assertion] =
      val clusterState = Coupled(setting)
      TestControl.executeEmbed:
        newClusterWatch(Some(clusterState)).flatMap { implicit watch =>
          for
            _ <- IO.sleep(duration)
            event = ClusterSwitchedOver(bId)
            switchedOver = SwitchedOver(setting.copy(activeId = bId))
            confirmed <- applyEvent(clusterState, aId, event, switchedOver)
            _ = assert(confirmed.isRight)
            _ = assert(watch.isActive(bId).orThrow)

            // Again
            confirmed <- applyEvent(clusterState, bId, event, switchedOver)
          yield
            assert(confirmed.isRight)
            assert(watch.isActive(bId).orThrow)
        }

    "applyEvent after event loss" in:
      val clusterState = Coupled(setting)

      // We test the loss of a PassiveLost event, and then apply Coupled
      val passiveLostEvent = ClusterPassiveLost(bId)
      val passiveLost = PassiveLost(setting.copy(activeId = aId))
      val prepared = PreparedToBeCoupled(setting.copy(activeId = aId))
      assert(clusterState.applyEvent(passiveLostEvent) == Right(passiveLost))

      TestControl.executeEmbed:
        newClusterWatch(Some(clusterState)).flatMap { implicit watch =>
          for
            confirmed <- watch.processRequest:
              ClusterWatchCheckEvent(RequestId(123), correlId, aId, passiveLostEvent, prepared)
            _ = assert(confirmed == Right(Confirmed()))
            nextEvent1 = ClusterCouplingPrepared(aId)
            _ = assert(passiveLost.applyEvent(nextEvent1) == Right(prepared))
            confirmed <- watch.processRequest:
              ClusterWatchCheckEvent(RequestId(123), correlId, aId, nextEvent1, prepared)
            _ = assert(confirmed == Right(Confirmed()))
            _ = assert(watch.clusterState() == Right(prepared))
            nextEvent2 = ClusterCoupled(aId)
            coupled = Coupled(setting.copy(activeId = aId))
            _ = assert(prepared.applyEvent(nextEvent2) == Right(coupled))
            confirmed <- watch.processRequest:
              ClusterWatchCheckEvent(RequestId(123), correlId, aId, nextEvent2, coupled)
          yield
            assert(confirmed == Right(Confirmed()))
            assert(watch.clusterState() == Right(coupled))
        }

    def applyEvent(
      clusterState: ClusterState,
      from: NodeId,
      event: ClusterEvent,
      expectedClusterState: HasNodes)
      (implicit watch: ClusterWatch)
    : IO[Checked[Confirmed]] =
      assert(expectedClusterState == clusterState.applyEvent(event).orThrow)
      watch.processRequest(ClusterWatchCheckEvent(RequestId(123), correlId, from, event, expectedClusterState))
        .<*(IO(
          assert(watch.clusterState() == Right(expectedClusterState))))

    def heartbeat(from: NodeId, clusterState: HasNodes)(implicit watch: ClusterWatch)
    : IO[Checked[Confirmed]] =
      watch.processRequest(ClusterWatchCheckState(RequestId(123), correlId, from, clusterState))
  }

  "ClusterPassiveLost when ClusterWatch is still untaught requires manual confirmation" in:
    val eventBus = new ClusterWatchEventBus
    val watch = new ClusterWatch(
      onUndecidableClusterNodeLoss = {
        case Some(problem) => IO(eventBus.publish(problem))
        case None => IO.unit
      })
    val passiveLost = PassiveLost(setting)
    import passiveLost.{activeId, passiveId}
    val event = ClusterPassiveLost(passiveId)

    val rejectedConfirmations = mutable.Buffer[ClusterNodeLossNotConfirmedProblem]()
    eventBus.subscribe[ClusterNodeLossNotConfirmedProblem](rejectedConfirmations += _)

    assert(watch.clusterState() == Left(UntaughtClusterWatchProblem))
    assert(watch.clusterNodeLossEventToBeConfirmed(aId) == None)
    assert(watch.clusterNodeLossEventToBeConfirmed(bId) == None)
    assert(watch.manuallyConfirmNodeLoss(passiveId, "CONFIRMER").await(99.s)
      == Left(ClusterNodeIsNotLostProblem(passiveId)))

    // Cluster node tries and fails
    assert(watch.processRequest(ClusterWatchCheckEvent(
      RequestId(123), correlId, activeId, event, passiveLost)).await(99.s)
      == Left(ClusterNodeLossNotConfirmedProblem(activeId, event)))
    assert(watch.clusterState() == Left(UntaughtClusterWatchProblem))
    assert(rejectedConfirmations == Seq(ClusterNodeLossNotConfirmedProblem(activeId, event)))

    // ClusterWatch remembers ClusterPassiveLost
    assert(watch.clusterNodeLossEventToBeConfirmed(passiveId) == Some(event))

    // Manually confirm node loss
    watch.manuallyConfirmNodeLoss(passiveId, "CONFIRMER").await(99.s).orThrow

    // Cluster node's second try succeeds
    assert(watch.processRequest(ClusterWatchCheckEvent(
      RequestId(123), correlId, activeId, event, passiveLost)).await(99.s)
      == Right(Confirmed(Some("CONFIRMER"))))
    assert(watch.clusterState() == Right(passiveLost))

    // Even a repetition succeeds, it's idempotent because ClusterState does not change
    assert(watch.processRequest(ClusterWatchCheckEvent(
      RequestId(123), correlId, activeId, event, passiveLost)).await(99.s)
      == Right(Confirmed()))

    // ClusterWatchCheckState succeeds
    assert(watch.processRequest(ClusterWatchCheckState(
      RequestId(123), correlId, activeId, passiveLost)).await(99.s)
      == Right(Confirmed()))

    assert(watch.clusterState() == Right(passiveLost))

  "ClusterFailedOver when ClusterWatch is still untaught requires manual confirmation" in:
    val watch = new ClusterWatch
    val failedOver = FailedOver(setting, failedAt)
    val lostNodeId = failedOver.passiveId
    val activatedId = failedOver.activeId
    val event = ClusterFailedOver(lostNodeId, activatedId, failedAt)

    assert(watch.clusterState() == Left(UntaughtClusterWatchProblem))
    assert(watch.clusterNodeLossEventToBeConfirmed(lostNodeId) == None)
    assert(watch.manuallyConfirmNodeLoss(lostNodeId, "CONFIRMER").await(99.s)
      == Left(ClusterNodeIsNotLostProblem(lostNodeId)))

    // Cluster node tries and fails
    assert(watch.processRequest(ClusterWatchCheckEvent(
      RequestId(123), correlId, activatedId, event, failedOver)).await(99.s)
      == Left(ClusterNodeLossNotConfirmedProblem(activatedId, event)))
    assert(watch.clusterState() == Left(UntaughtClusterWatchProblem))

    // ClusterWatch remembers ClusterFailedOver
    assert(watch.clusterNodeLossEventToBeConfirmed(lostNodeId) == Some(event))

    // Manually confirm node loss
    watch.manuallyConfirmNodeLoss(lostNodeId, "CONFIRMER").await(99.s).orThrow

    // Cluster node's second try succeeds
    assert(watch.processRequest(ClusterWatchCheckEvent(
      RequestId(123), correlId, activatedId, event, failedOver)).await(99.s)
      == Right(Confirmed(Some("CONFIRMER"))))
    assert(watch.clusterNodeLossEventToBeConfirmed(lostNodeId) == None)
    assert(watch.clusterState() == Right(failedOver))

    // Even a repetition succeeds, it's idempotent because ClusterState does not change
    assert(watch.processRequest(ClusterWatchCheckEvent(
      RequestId(123), correlId, activatedId, event, failedOver)).await(99.s)
      == Right(Confirmed(None)))

    // ClusterWatchCheckState succeeds
    assert(watch.processRequest(ClusterWatchCheckState(
      RequestId(123), correlId, activatedId, failedOver)).await(99.s)
      == Right(Confirmed(None)))

    assert(watch.clusterState() == Right(failedOver))
    assert(watch.clusterNodeLossEventToBeConfirmed(lostNodeId) == None)

  "requireManualNodeLossConfirmation" - {
    "ClusterFailedOver" in:
      import setting.{activeId, passiveId}
      checkRequireNodeLossConfirm(from = passiveId, ClusterFailedOver(activeId, activatedId = passiveId, failedAt))

    //"ClusterPassiveLost" in {
    //  import setting.{activeId, passiveId}
    //  checkRequireNodeLossConfirm(from = activeId, ClusterPassiveLost(passiveId))
    //}

    def checkRequireNodeLossConfirm(from: NodeId, event: ClusterFailedOver): IO[Assertion] =
      val coupled = Coupled(setting)
      import coupled.{activeId, passiveId}
      import event.lostNodeId

      TestControl.executeEmbed:
        implicit val watch = new ClusterWatch(
          requireManualNodeLossConfirmation = true,
          onUndecidableClusterNodeLoss = _ => IO.unit)

        for
          // Initialize ClusterWatch
          _ <- watch
            .processRequest(ClusterWatchCheckState(RequestId(123), correlId, activeId, coupled))
            .map(_.orThrow)

          confirmed <- watch.manuallyConfirmNodeLoss(activeId, "CONFIRMER")
          _ = assert(confirmed == Left(ClusterNodeIsNotLostProblem(activeId)))

          confirmed <- watch.manuallyConfirmNodeLoss(passiveId, "CONFIRMER")
          _ = assert(confirmed == Left(ClusterNodeIsNotLostProblem(passiveId)))

          _ <- IO.sleep(setting.timing.clusterWatchHeartbeatValidDuration)
          confirmed <- watch.manuallyConfirmNodeLoss(activeId, "CONFIRMER")
          _ = assert(confirmed == Left(ClusterNodeIsNotLostProblem(activeId)))

          confirmed <- watch.manuallyConfirmNodeLoss(passiveId, "CONFIRMER")
          _ = assert(confirmed == Left(ClusterNodeIsNotLostProblem(passiveId)))

          expectedClusterState = coupled.applyEvent(event).orThrow.asInstanceOf[HasNodes]

          // Event is rejected because Node loss has not yet been confirmed
          response <- watch.processRequest:
            ClusterWatchCheckEvent(RequestId(123), correlId, from, event, expectedClusterState)
          _ = assert(response == Left(ClusterNodeLossNotConfirmedProblem(from, event)))
          _ = assert(watch.clusterState() == Right(coupled))

          // Try to confirm a loss of the not lost Node
          notLostNodeId = setting.other(lostNodeId)
          confirmed <- watch.manuallyConfirmNodeLoss(notLostNodeId, "CONFIRMER")
          _ = assert(confirmed == Left(ClusterNodeIsNotLostProblem(notLostNodeId)))

          // Confirm the loss of the Node
          _ <- watch.manuallyConfirmNodeLoss(lostNodeId, "CONFIRMER").map(_.orThrow)

          //IO.sleep(setting.timing.clusterWatchHeartbeatValidDuration)
          _ <- watch
            .processRequest:
              ClusterWatchCheckEvent(RequestId(123), correlId, notLostNodeId, event, expectedClusterState)
            .map(_.orThrow)
          _ = assert(watch.clusterState() == Right(expectedClusterState))

          confirmed <- watch.manuallyConfirmNodeLoss(activeId, "CONFIRMER")
          _ = assert(confirmed == Left(ClusterNodeIsNotLostProblem(activeId)))

          confirmed <- watch.manuallyConfirmNodeLoss(passiveId, "CONFIRMER")
        yield
          assert(confirmed == Left(ClusterNodeIsNotLostProblem(passiveId)))
  }
