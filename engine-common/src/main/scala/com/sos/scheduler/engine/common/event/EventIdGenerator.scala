package com.sos.scheduler.engine.common.event

import com.sos.scheduler.engine.data.event.{EventId, Snapshot}
import java.lang.System._
import java.util.concurrent.atomic.AtomicLong
import javax.inject.{Inject, Singleton}
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class EventIdGenerator @Inject extends Iterator[EventId] {

  private val lastResult = new AtomicLong(EventId.BeforeFirst)

  def lastUsedEventId: EventId = lastResult.get

  def hasNext = true

  @tailrec
  def next(): EventId = {
    val nowId = currentTimeMillis * EventId.IdsPerMillisecond
    val last = lastResult.get
    val nextId = if (last < nowId) nowId else last + 1
    if (lastResult.compareAndSet(last, nextId))
      nextId
    else
      next()
  }

  def newSnapshot[A](a: A) = Snapshot(next(), a)
}
