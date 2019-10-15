package com.sos.jobscheduler.core.event.journal.data

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.data.event.{Event, KeyedEventTypedJsonCodec}
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
  val name = fileBase.getFileName.toString
}
