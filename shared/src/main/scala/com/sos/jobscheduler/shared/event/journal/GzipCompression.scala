package com.sos.scheduler.engine.shared.event.journal

import java.io.{InputStream, OutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

/**
  * @author Joacim Zschimmer
  */
trait GzipCompression {
  this: JsonJournalMeta ⇒

  override protected def convertOutputStream(out: OutputStream) =
    new GZIPOutputStream(out, /*syncFlush=*/true)

  override protected def convertInputStream(in: InputStream) =
    new GZIPInputStream(in)
}
