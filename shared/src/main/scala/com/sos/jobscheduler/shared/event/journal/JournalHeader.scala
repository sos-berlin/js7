package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}

/**
  * @author Joacim Zschimmer
  */
final case class JournalHeader(
  version: String,
  softwareVersion: String)

object JournalHeader {

  implicit lazy val JsonCodec = TypedJsonCodec[JournalHeader](
    Subtype.named(deriveCirceCodec[JournalHeader], "JobScheduler.Journal"))
}
