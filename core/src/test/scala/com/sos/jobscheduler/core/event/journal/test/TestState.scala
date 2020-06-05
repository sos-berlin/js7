package js7.core.event.journal.test

import js7.base.problem.Checked
import js7.data.event.{Event, EventId, JournalState, JournaledState, KeyedEvent}
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
final case class TestState(
  standards: JournaledState.Standards = JournaledState.Standards.empty,
  keyToAggregate: Map[String, TestAggregate])
extends JournaledState[TestState]
{
  override def toSnapshotObservable =
    standards.toSnapshotObservable ++
      Observable.fromIterable(keyToAggregate.values)

  def applySnapshot(snapshot: Any): TestState =
    snapshot match {
      case o: TestAggregate =>
        copy(keyToAggregate =
          keyToAggregate + (o.key -> o))

      case o: JournalState =>
        copy(standards = standards.copy(
          journalState = o))
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
    this

  def withStandards(standards: JournaledState.Standards) =
    copy(standards = standards)
}

object TestState
{
  val empty = TestState(JournaledState.Standards.empty, Map.empty)
}
