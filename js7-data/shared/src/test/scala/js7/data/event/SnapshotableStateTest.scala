package js7.data.event
import js7.base.auth.UserId
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.data.cluster.{ClusterEvent, ClusterSetting, ClusterState, ClusterTiming}
import js7.data.event.EventDrivenState.EventNotApplicableProblem
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.SnapshotableStateTest.*
import js7.data.node.NodeId
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SnapshotableStateTest extends AnyFreeSpec
{
  private var s = MyState.empty

  "applyStandardEvent and applyEvents" in {
    s = s.applyEvent(NoKey <-: ClusterEvent.ClusterNodesAppointed(setting)).orThrow

    s = s.applyEvents(
      (NoKey <-: ClusterEvent.ClusterCouplingPrepared(primaryNodeId)) ::
      (NoKey <-: ClusterEvent.ClusterCoupled(primaryNodeId)) ::
      (NoKey <-: JournalEvent.JournalEventsReleased(UserId("USER"), EventId(333))) ::
      Nil
    ).orThrow

    assert(s == MyState(0L, SnapshotableState.Standards(
      JournalState(Map(UserId("USER") -> EventId(333))),
      ClusterState.Coupled(setting))))
  }

  "EventNotApplicableProblem" in {
    val invalidEvent = NoKey <-: new NoKeyEvent {}
    assert(s.applyEvent(invalidEvent) == Left(EventNotApplicableProblem(invalidEvent, s)))
  }
}

private object SnapshotableStateTest
{
  private val primaryNodeId = NodeId("PRIMARY")
  private val setting = ClusterSetting(
    Map(
      primaryNodeId -> Uri("https://PRIMARY"),
      NodeId("BACKUP") -> Uri("https://BACKUP")),
    primaryNodeId,
    Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
    ClusterTiming(10.s, 20.s))

  private case class MyState(eventId: EventId, standards: SnapshotableState.Standards)
  extends SnapshotableState[MyState]
  {
    def companion = MyState

    def estimatedSnapshotSize = standards.snapshotSize

    def toSnapshotObservable = standards.toSnapshotObservable

    def withStandards(standards: SnapshotableState.Standards) =
      copy(standards = standards)

    def applyEvent(keyedEvent: KeyedEvent[Event]) =
      applyStandardEvent(keyedEvent)

    def withEventId(eventId: EventId) = copy(eventId = eventId)
  }
  private object MyState extends SnapshotableState.Companion[MyState] {
    type StateEvent = Event

    val empty = MyState(EventId.BeforeFirst, SnapshotableState.Standards.empty)

    // TODO Refactor this into a separate common trait
    protected def inventoryItems = throw new NotImplementedError
    def snapshotObjectJsonCodec = throw new NotImplementedError
    implicit def keyedEventJsonCodec = throw new NotImplementedError
    def newBuilder() = throw new NotImplementedError
  }
}
