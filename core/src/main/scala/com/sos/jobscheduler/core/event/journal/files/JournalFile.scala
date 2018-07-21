package com.sos.jobscheduler.core.event.journal.files

import com.sos.jobscheduler.data.event.EventId
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[journal] final case class JournalFile private[journal](afterEventId: EventId, file: Path)
{
  override def toString = file.getFileName.toString
}
