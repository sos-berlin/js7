package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.data.event.{Event, KeyedEventTypedJsonCodec}

/**
  * @author Joacim Zschimmer
  */
class JournalMeta[E <: Event](
  val snapshotJsonCodec: TypedJsonCodec[Any],
  implicit val eventJsonCodec: KeyedEventTypedJsonCodec[E])
extends StreamConversion

object JournalMeta {
  val Header = JournalHeader(version = "0.5",   // TODO Vor der ersten Software-Freigabe zu "1" wechseln
    softwareVersion = BuildInfo.version)

  def gzipped[E <: Event](
    snapshotJsonCodec: TypedJsonCodec[Any],
    eventJsonCodec: KeyedEventTypedJsonCodec[E],
    compressWithGzip: Boolean = true)
  : JournalMeta[E] = {
    val gz = compressWithGzip
    new JournalMeta(snapshotJsonCodec, eventJsonCodec) with GzipCompression {
      override def compressWithGzip = gz
    }
  }
}
