package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.data.event.EventId
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[journal] trait JournalingObserver
{
  private[journal] def onJournalingStarted(file: Path, flushedLengthAndEventId: PositionAnd[EventId]): Unit

  private[journal] def onEventsAdded(flushedPositionAndEventId: PositionAnd[EventId]): Unit
}
