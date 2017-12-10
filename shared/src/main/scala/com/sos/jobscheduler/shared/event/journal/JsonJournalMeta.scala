package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.data.event.{Event, KeyedEventTypedJsonCodec}

/**
  * @author Joacim Zschimmer
  */
class JsonJournalMeta[E <: Event](
  val snapshotJsonCodec: TypedJsonCodec[Any],
  implicit val eventJsonCodec: KeyedEventTypedJsonCodec[E])
extends StreamConversion

object JsonJournalMeta {
  val Header = JsonJournalHeader(
    version = "0.1",   // TODO Vor der ersten Software-Freigabe zu "1" wechseln
    softwareVersion = BuildInfo.version)

  def gzipped[E <: Event](
    snapshotJsonCodec: TypedJsonCodec[Any],
    eventJsonCodec: KeyedEventTypedJsonCodec[E],
    compressWithGzip: Boolean = true)
  : JsonJournalMeta[E] = {
    val gz = compressWithGzip
    new JsonJournalMeta(snapshotJsonCodec, eventJsonCodec) with GzipCompression {
      override def compressWithGzip = gz
    }
  }
}
