package js7.journal

import java.util.concurrent.ConcurrentHashMap
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.data.event.EventId
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

/**
  * @author Joacim Zschimmer
  */
final class EventIdGeneratorTest extends AnyFreeSpec
{
  "test" in {
    val eventIds: mutable.Map[EventId, Unit] = new ConcurrentHashMap[EventId, Unit].asScala
    val eventIdGenerator = new EventIdGenerator
    val n = 10000 * sys.runtime.availableProcessors
    (for (_ <- 1 to n) yield
      Future {
        eventIds += ((eventIdGenerator.next(), ()))
      }) await 99.s
    assert(eventIds.size == n)  // All EventIds are distinct
  }

  "updateLastEventId" in {
    val eventIdGenerator = new EventIdGenerator(EventIdClock.fixed(epochMilli = 100))
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
