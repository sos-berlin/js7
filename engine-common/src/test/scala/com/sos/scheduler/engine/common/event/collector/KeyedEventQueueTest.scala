package com.sos.scheduler.engine.common.event.collector

import com.sos.scheduler.engine.common.event.collector.KeyedEventQueueTest._
import com.sos.scheduler.engine.data.event.{EventId, KeyedEvent, NoKeyEvent, Snapshot}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class KeyedEventQueueTest extends FreeSpec {

  private val queue = new KeyedEventQueue(3)
  private val eventSnapshots = for (i ← 1 to 5) yield Snapshot(EventId(i), KeyedEvent(AEvent(i)))

  eventSnapshots foreach queue.add

  "event stream is torn - the first event has been lost" in {
    assert(queue.after(EventId.BeforeFirst) == None)
  }

  "hasAfter" in {
    assert(queue.hasAfter(EventId.BeforeFirst))
    assert(queue.hasAfter(EventId(1)))
    assert(queue.hasAfter(EventId(2)))
    assert(queue.hasAfter(EventId(3)))
    assert(queue.hasAfter(EventId(4)))
    assert(!queue.hasAfter(EventId(5)))
  }

  "after" in {
    //assert(queue.events(EventId(1)) == None)
    assert((queue.after(EventId(2)) map { _.toVector }) == Some(eventSnapshots drop 2))
    assert((queue.after(EventId(3)) map { _.toVector }) == Some(eventSnapshots drop 3))
    assert((queue.after(EventId(4)) map { _.toVector }) == Some(eventSnapshots drop 4))
    assert(queue.after(EventId(5)).get.isEmpty)
  }

  "reverseEvents" in {
    assert(queue.reverseEvents(EventId.BeforeFirst).toVector == (eventSnapshots drop 2).reverse)
    assert(queue.reverseEvents(EventId(1)).toVector == (eventSnapshots drop 2).reverse)
    assert(queue.reverseEvents(EventId(2)).toVector == (eventSnapshots drop 2).reverse)
    assert(queue.reverseEvents(EventId(3)).toVector == (eventSnapshots drop 3).reverse)
    assert(queue.reverseEvents(EventId(4)).toVector == (eventSnapshots drop 4).reverse)
    assert(queue.reverseEvents(EventId(5)).isEmpty)
  }
}

object KeyedEventQueueTest {
  private case class AEvent(number: Int) extends NoKeyEvent
}
