package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.base.sprayjson.typed.TypedJsonFormat
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.data.event.{Event, KeyedTypedEventJsonFormat}

/**
  * @author Joacim Zschimmer
  */
class JsonJournalMeta(
  val snapshotJsonFormat: TypedJsonFormat[Any],
  implicit val eventJsonFormat: KeyedTypedEventJsonFormat[Event],
  val snapshotToKey: Any ⇒ Any,
  val isDeletedEvent: Event ⇒ Boolean)
extends StreamConversion

object JsonJournalMeta {
  val Header = JsonJournalHeader(version = "1", softwareVersion = BuildInfo.version)
}
