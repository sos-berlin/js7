package com.sos.scheduler.engine.common.event

import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.data.event.EventId
import java.util.concurrent.ConcurrentHashMap
import org.scalatest.FreeSpec
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class EventIdGeneratorTest extends FreeSpec {

  "test" in {
    val eventIds: mutable.Map[EventId, Unit] = new ConcurrentHashMap[EventId, Unit]
    val uniqueTimestampedIdIterator = new EventIdGenerator
    val n = 10000 * sys.runtime.availableProcessors
    (for (_ ← 1 to n) yield
      Future {
        eventIds += uniqueTimestampedIdIterator.next() → ()
      }) await 20.s
    assert(eventIds.size == n)  // All EventIds are distinct
  }
}
