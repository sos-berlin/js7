package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.data.event.{Event, KeyedEventTypedJsonCodec}

/**
  * @author Joacim Zschimmer
  */
class JournalMeta[E <: Event](
  val snapshotJsonCodec: TypedJsonCodec[Any],
  implicit val eventJsonCodec: KeyedEventTypedJsonCodec[E])

object JournalMeta {
  def header = JournalHeader(
    version = "0.12",   // TODO Vor der ersten Software-Freigabe zu "1" wechseln
    softwareVersion = BuildInfo.version,
    buildId = BuildInfo.buildId,
    Timestamp.now.toIsoString)
}
