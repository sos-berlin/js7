package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEventTypedJsonCodec}
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final case class JournalMeta[E <: Event](
  snapshotJsonCodec: TypedJsonCodec[Any],
  implicit val eventJsonCodec: KeyedEventTypedJsonCodec[E],
  /** Path without extension, like "/directory/test". */
  fileBase: Path)
{
  def file(after: EventId, extraSuffix: String = ""): Path =
    JournalFiles.journalFile(fileBase, after, extraSuffix)
}
