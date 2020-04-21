package com.sos.jobscheduler.data.event
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterNodeId, ClusterState}
import com.sos.jobscheduler.data.event.JournaledState.EventNotApplicableProblem
import com.sos.jobscheduler.data.event.JournaledStateTest.{MyState, _}
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournaledStateTest extends AnyFreeSpec
{
  private var s = MyState.empty

  "applyStandardEvent and applyEvents" in {
    s = s.applyEvent(NoKey <-: ClusterEvent.ClusterNodesAppointed(idToNode, primaryNodeId)).orThrow

    s = s.applyEvents(
      (NoKey <-: ClusterEvent.ClusterCouplingPrepared(primaryNodeId)) ::
      (NoKey <-: ClusterEvent.ClusterCoupled(primaryNodeId)) ::
      (NoKey <-: JournalEvent.JournalEventsReleased(UserId("USER"), EventId(333))) ::
      Nil
    ).orThrow

    assert(s == MyState(JournaledState.Standards(
      JournalState(Map(UserId("USER") -> EventId(333))),
      ClusterState.Coupled(idToNode, primaryNodeId))))
  }

  "EventNotApplicableProblem" in {
    val invalidEvent = NoKey <-: new NoKeyEvent {}
    assert(s.applyEvent(invalidEvent) == Left(EventNotApplicableProblem(invalidEvent, s)))
  }
}

private object JournaledStateTest
{
  private val primaryNodeId = ClusterNodeId("Primary")
  private val idToNode = Map(
    primaryNodeId -> Uri("http://PRIMARY"),
    ClusterNodeId("Backup") -> Uri("http://BACKUP"))

  private case class MyState(standards: JournaledState.Standards) extends JournaledState[MyState, Event]
  {
    def toSnapshotObservable = standards.toSnapshotObservable

    protected def withStandards(standards: JournaledState.Standards) =
      copy(standards = standards)

    def applyEvent(keyedEvent: KeyedEvent[Event]) =
      applyStandardEvent(keyedEvent)

    def withEventId(eventId: EventId) = throw new NotImplementedError
  }
  private object MyState {
    val empty = MyState(JournaledState.Standards.empty)
  }
}
