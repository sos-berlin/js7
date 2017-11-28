package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}

/**
  * @author Joacim Zschimmer
  */
final case class JsonJournalHeader(
  version: String,
  softwareVersion: String)

object JsonJournalHeader {

  implicit lazy val JsonCodec = TypedJsonCodec[JsonJournalHeader](
    Subtype.named(deriveCirceCodec[JsonJournalHeader], "JobScheduler.Journal"))
}
