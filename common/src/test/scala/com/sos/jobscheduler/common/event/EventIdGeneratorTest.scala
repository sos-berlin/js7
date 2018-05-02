package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.common.event
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.EventId
import java.util.concurrent.ConcurrentHashMap
import org.scalatest.FreeSpec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class EventIdGeneratorTest extends FreeSpec {

  "test" in {
    val eventIds: mutable.Map[EventId, Unit] = new ConcurrentHashMap[EventId, Unit].asScala
    val eventIdGenerator = new EventIdGenerator
    val n = 10000 * sys.runtime.availableProcessors
    (for (_ ← 1 to n) yield
      Future {
        eventIds += ((eventIdGenerator.next(), ()))
      }) await 20.s
    assert(eventIds.size == n)  // All EventIds are distinct
  }

  "updateLastEventId" in {
    val eventIdGenerator = new EventIdGenerator(new event.EventIdClock.Fixed(currentTimeMillis = 100))
    assert(eventIdGenerator.lastUsedEventId == 0)
    assert(eventIdGenerator.next() == 100000)

    eventIdGenerator.updateLastEventId(100)
    assert(eventIdGenerator.lastUsedEventId == 100000)
    assert(eventIdGenerator.next() == 100001)

    eventIdGenerator.updateLastEventId(200000)
    assert(eventIdGenerator.lastUsedEventId == 200000)
    assert(eventIdGenerator.next() == 200001)
  }
}
