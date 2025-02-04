package js7.data.event

import js7.base.auth.UserId
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.data.cluster.{ClusterEvent, ClusterSetting, ClusterState, ClusterTiming, ClusterWatchId}
import js7.data.event.ClusterableStateTest.*
import js7.data.event.EventDrivenState.EventNotApplicableProblem
import js7.data.event.KeyedEvent.NoKey
import js7.data.node.NodeId

/**
  * @author Joacim Zschimmer
  */
final class ClusterableStateTest extends OurTestSuite:
  private var s = MyState.empty

  "applyStandardEvent and applyKeyedEvents" in:
    s = s.applyKeyedEvent(NoKey <-: ClusterEvent.ClusterNodesAppointed(setting)).orThrow

    s = s.applyKeyedEvents(
      (NoKey <-: ClusterEvent.ClusterCouplingPrepared(primaryNodeId)) ::
      (NoKey <-: ClusterEvent.ClusterCoupled(primaryNodeId)) ::
      (NoKey <-: JournalEvent.JournalEventsReleased(UserId("USER"), EventId(333))) ::
      Nil
    ).orThrow

    assert(s == MyState(0L, SnapshotableState.Standards(
      JournalState(Map(UserId("USER") -> EventId(333))),
      ClusterState.Coupled(setting))))

  "EventNotApplicableProblem" in:
    val invalidEvent = NoKey <-: MyEvent
    assert(s.applyKeyedEvent(invalidEvent) == Left(EventNotApplicableProblem(invalidEvent, s)))

private object ClusterableStateTest:
  private val primaryNodeId = NodeId("PRIMARY")
  private val setting = ClusterSetting(
    Map(
      primaryNodeId -> Uri("https://PRIMARY"),
      NodeId("BACKUP") -> Uri("https://BACKUP")),
    primaryNodeId,
    ClusterTiming(10.s, 20.s),
    Some(ClusterWatchId("CLUSTER-WATCH")))

  private case class MyState(eventId: EventId, standards: SnapshotableState.Standards)
  extends ClusterableState[MyState]:
    def companion = MyState

    def name = "MyState"

    def estimatedSnapshotSize = standards.snapshotSize

    def toSnapshotStream = standards.toSnapshotStream

    def withStandards(standards: SnapshotableState.Standards) =
      copy(standards = standards)

    def applyKeyedEvent(keyedEvent: KeyedEvent[Event]) =
      applyStandardEvent(keyedEvent)

    def withEventId(eventId: EventId) = copy(eventId = eventId)

    def clusterNodeIdToName(nodeId: NodeId) =
      Left(Problem("clusterNodeIdToName not implemented"))

    def clusterNodeToUserId(nodeId: NodeId) =
      Left(Problem("clusterNodeToUserId not implemented"))
  private object MyState extends ClusterableState.Companion[MyState]:
    val empty = MyState(EventId.BeforeFirst, SnapshotableState.Standards.empty)

    // TODO Refactor this into a separate common trait
    def snapshotObjectJsonCodec = throw new NotImplementedError
    implicit def keyedEventJsonCodec = throw new NotImplementedError
    def newRecoverer() = throw new NotImplementedError

  private case object MyEvent extends NoKeyEvent
