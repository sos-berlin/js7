package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.write.EventJournalWriter
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, KeyedEventTypedJsonCodec, Stamped}
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
private[watch] object TestData {
  sealed trait TestEvent extends Event {
    type Key = String
  }

  final case object AEvent extends TestEvent
  final case object BEvent extends TestEvent

  implicit val jsonFormat = TypedJsonCodec[TestEvent](
    Subtype(AEvent),
    Subtype(BEvent))

  val TestKeyedEventJsonCodec = KeyedEventTypedJsonCodec[TestEvent](
    KeyedSubtype[TestEvent])

  def writeJournal[E <: Event](journalMeta: JournalMeta[E], after: EventId, stampedEvents: Seq[Stamped[KeyedEvent[E]]]): Unit =
    autoClosing(EventJournalWriter.forTest[E](journalMeta, after = after)) { writer ⇒
      writer.beginEventSection()
      writer.writeEvents(stampedEvents)
      writer.endEventSection(sync = false)
      writer.close()
    }
}
