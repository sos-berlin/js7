package js7.data.event
import js7.base.auth.UserId
import js7.base.problem.Checked.Ops
import js7.base.web.Uri
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.event.JournaledState.EventNotApplicableProblem
import js7.data.event.JournaledStateTest._
import js7.data.event.KeyedEvent.NoKey
import js7.data.node.NodeId
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

    assert(s == MyState(0L, JournaledState.Standards(
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
  private val primaryNodeId = NodeId("Primary")
  private val idToNode = Map(
    primaryNodeId -> Uri("http://PRIMARY"),
    NodeId("Backup") -> Uri("http://BACKUP"))

  private case class MyState(eventId: EventId, standards: JournaledState.Standards)
  extends JournaledState[MyState]
  {
    def estimatedSnapshotSize = standards.snapshotSize

    def toSnapshotObservable = standards.toSnapshotObservable

    protected def withStandards(standards: JournaledState.Standards) =
      copy(standards = standards)

    def applyEvent(keyedEvent: KeyedEvent[Event]) =
      applyStandardEvent(keyedEvent)

    def withEventId(eventId: EventId) = copy(eventId = eventId)
  }
  private object MyState {
    val empty = MyState(EventId.BeforeFirst, JournaledState.Standards.empty)
  }
}
