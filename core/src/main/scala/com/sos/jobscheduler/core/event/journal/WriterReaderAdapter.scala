package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.data.event.EventId
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[journal] trait WriterReaderAdapter
{
  private[journal] def onJournalingStarted(file: Path, flushedLengthAndEventId: PositionAnd[EventId]): Unit

  private[journal] def onEventsAdded(flushedPositionAndEventId: PositionAnd[EventId]): Unit
}
