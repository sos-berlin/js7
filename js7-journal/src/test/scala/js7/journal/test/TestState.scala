package js7.journal.test

import fs2.Stream
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, JournalEvent, KeyedEvent, KeyedEventTypedJsonCodec, SnapshotableState, SnapshotableStateRecoverer}

/**
  * @author Joacim Zschimmer
  */
final case class TestState(
  eventId: EventId,
  standards: SnapshotableState.Standards = SnapshotableState.Standards.empty,
  keyToAggregate: Map[String, TestAggregate])
extends SnapshotableState[TestState]:

  def companion = TestState

  def name = "TestState"

  def estimatedSnapshotSize = standards.snapshotSize + keyToAggregate.size

  def toSnapshotStream =
    standards.toSnapshotStream ++
      Stream.iterable(keyToAggregate.values)

  def applyKeyedEvent(keyedEvent: KeyedEvent[Event]): Checked[TestState] =
    keyedEvent match
      case KeyedEvent(key: String, event: TestEvent.Added) =>
        assert(!keyToAggregate.contains(key))
        import event.{a, b, c, d, e, f, g, h, i, k, l, m, n, o, p, q, r, string}
        Right(copy(
          keyToAggregate = keyToAggregate + (key -> TestAggregate(key, string, a, b, c, d, e, f, g, h, i, k, l, m, n, o, p, q, r))))

      case KeyedEvent(key: String, TestEvent.Removed) =>
        Right(copy(
          keyToAggregate = keyToAggregate - key))

      case KeyedEvent(key: String, event: TestEvent) =>
        Right(copy(
          keyToAggregate = keyToAggregate + (key -> keyToAggregate(key).applyEvent(event))))

      case keyedEvent =>
        applyStandardEvent(keyedEvent)

  def withEventId(eventId: EventId): TestState =
    copy(eventId = eventId)

  def withStandards(standards: SnapshotableState.Standards) =
    copy(standards = standards)


object TestState extends SnapshotableState.Companion[TestState]:
  val empty = TestState(EventId.BeforeFirst, SnapshotableState.Standards.empty, Map.empty)

  def newRecoverer() = new SnapshotableStateRecoverer.Simple(TestState):
    def onAddSnapshotObject =
      case o: TestAggregate =>
        updateState(state.copy(keyToAggregate =
          state.keyToAggregate + (o.key -> o)))

  def snapshotObjectJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec[Any](
      Subtype[TestAggregate])

  implicit def keyedEventJsonCodec =
    KeyedEventTypedJsonCodec[Event](
        KeyedSubtype[JournalEvent],
        KeyedSubtype[TestEvent])

  protected def inventoryItems = Nil
