package js7.core.event.journal.test

import js7.base.problem.Checked
import js7.data.event.{Event, EventId, JournaledState, KeyedEvent}
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
final case class TestState(
  eventId: EventId,
  standards: JournaledState.Standards = JournaledState.Standards.empty,
  keyToAggregate: Map[String, TestAggregate])
extends JournaledState[TestState]
{
  def estimatedSnapshotSize = standards.snapshotSize + keyToAggregate.size

  def toSnapshotObservable =
    standards.toSnapshotObservable ++
      Observable.fromIterable(keyToAggregate.values)

  override def applySnapshotObject(obj: Any) =
    obj match {
      case o: TestAggregate =>
        Right(copy(keyToAggregate =
          keyToAggregate + (o.key -> o)))

      case o => super.applySnapshotObject(o)
    }

  def applyEvent(keyedEvent: KeyedEvent[Event]): Checked[TestState] =
    keyedEvent match {
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
    }

  def withEventId(eventId: EventId): TestState =
    copy(eventId = eventId)

  def withStandards(standards: JournaledState.Standards) =
    copy(standards = standards)
}

object TestState extends JournaledState.Companion[TestState]
{
  val empty = TestState(EventId.BeforeFirst, JournaledState.Standards.empty, Map.empty)

  def name = "TestState"

  def fromObservable(snapshotObjects: Observable[Any]) =
    throw new NotImplementedError

  implicit def snapshotObjectJsonCodec =
    throw new NotImplementedError

  implicit def keyedEventJsonDecoder =
    throw new NotImplementedError
}
