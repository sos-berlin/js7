package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.base.sprayjson.typed.TypedJsonFormat
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, KeyedTypedEventJsonFormat, Stamped}
import com.sos.jobscheduler.shared.event.journal.JsonJournalRecoverer.SnapshotRecovered
import spray.json.JsValue

/**
  * @author Joacim Zschimmer
  */
class JsonJournalMeta[E <: Event](
  val snapshotJsonFormat: TypedJsonFormat[Any],
  implicit val eventJsonFormat: KeyedTypedEventJsonFormat[E],
  val snapshotToKey: Any ⇒ Any,
  val isDeletedEvent: E ⇒ Boolean)
extends StreamConversion {

  def deserialize(journalEntry: JsValue): Any =
    if (eventJsonFormat canDeserialize journalEntry.asJsObject)
      journalEntry.convertTo[Stamped[KeyedEvent[E]]]
    else
      SnapshotRecovered(snapshotJsonFormat.read(journalEntry))
}

object JsonJournalMeta {
  val Header = JsonJournalHeader(
    version = "0.0",   // TODO Vor der ersten Software-Freigabe zu "1" wechseln
    softwareVersion = BuildInfo.version)
}
