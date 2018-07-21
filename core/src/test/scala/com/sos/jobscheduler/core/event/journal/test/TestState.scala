package com.sos.jobscheduler.core.event.journal.test

import com.sos.jobscheduler.data.event.KeyedEvent

/**
  * @author Joacim Zschimmer
  */
final case class TestState(keyToAggregate: Map[String, TestAggregate])
{
  def applySnapshot(aggregate: TestAggregate): TestState =
    TestState(keyToAggregate + (aggregate.key → aggregate))

  def applyEvent(keyedEvent: KeyedEvent[TestEvent]): TestState = {
    val KeyedEvent(key, event) = keyedEvent
    event match {
      case event: TestEvent.Added ⇒
        assert(!keyToAggregate.contains(key))
        import event._
        TestState(keyToAggregate + (key → TestAggregate(key, string, a, b, c, d, e, f, g, h, i, k, l, m, n, o, p, q, r)))

      case TestEvent.Removed ⇒
        TestState(keyToAggregate - key)

      case event: TestEvent ⇒
        TestState(keyToAggregate + (key → keyToAggregate(key).applyEvent(event)))
    }
  }
}
