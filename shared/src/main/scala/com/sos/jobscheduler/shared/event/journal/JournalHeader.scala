package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}

/**
  * @author Joacim Zschimmer
  */
final case class JournalHeader(
  version: String,
  softwareVersion: String)

object JournalHeader {

  implicit lazy val jsonCodec = TypedJsonCodec[JournalHeader](
    Subtype.named(deriveCodec[JournalHeader], "JobScheduler.Journal"))
}
