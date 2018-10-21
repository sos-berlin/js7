package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.common.scalautil.FileUtils
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.{JournalHeaders, JournalMeta}
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.watch.FileEventIteratorTest._
import com.sos.jobscheduler.core.event.journal.watch.TestData.{AEvent, TestEvent, TestKeyedEventJsonCodec, writeJournal}
import com.sos.jobscheduler.data.event.{EventId, Stamped}
import io.circe.syntax.EncoderOps
import java.nio.file.Files
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
  * @author Joacim Zschimmer
  */
final class FileEventIteratorTest extends FreeSpec
{
  "FileEventIterator" in {
    FileUtils.withTemporaryDirectory("FileEventIteratorPoolTest-") { dir ⇒
      val journalMeta = new JournalMeta[TestEvent](TypedJsonCodec[Any](), TestKeyedEventJsonCodec, dir resolve "test")
      val journalFile = journalMeta.file(after = After)
      writeJournal[TestEvent](journalMeta, after = After, TestEvents)

      val iterator = new FileEventIterator[TestEvent](journalMeta, journalFile, tornEventId = After, () ⇒ Files.size(journalFile))
      iterator.firstEventPosition  // Must be called before reading
      iterator.next()
      val firstPos = PositionAnd(iterator.firstEventPosition, After)
      val secondPos = PositionAnd(iterator.position, TestEvents(0).eventId)

      for (_ ← 1 to 3) {
        iterator.seek(firstPos)
        assert(iterator.next() == TestEvents(0))
        assert(iterator.next() == TestEvents(1))
        assert(iterator.next() == TestEvents(2))
        assert(iterator.next() == TestEvents(3))
        assert(!iterator.hasNext)
      }

      intercept[IllegalArgumentException] {
        iterator.seek(PositionAnd(iterator.firstEventPosition, After - 1))
      }

      iterator.seek(secondPos)
      iterator.next() shouldEqual TestEvents(1)

      iterator.seek(secondPos)
      assert(iterator.hasNext)
      iterator.next() shouldEqual TestEvents(1)

      iterator.seek(firstPos)
      assert(iterator.hasNext)
      iterator.seek(secondPos)
      iterator.next() shouldEqual TestEvents(1)

      iterator.seek(secondPos)
      object onOneSkipped extends (PositionAnd[EventId] ⇒ Unit) {
        var eventId: EventId = -1
        def apply(positionAndEventId: PositionAnd[EventId]) = eventId = positionAndEventId.value
        def check(eventId: EventId = -1) = {
          assert(eventId == this.eventId)
          this.eventId = -1
        }
      }
      iterator.skipToEventAfter(After)(onOneSkipped) shouldEqual false
      onOneSkipped.check()
      iterator.next() shouldEqual TestEvents(1)

      iterator.seek(firstPos)
      iterator.skipToEventAfter(After + 15)(onOneSkipped) shouldEqual false
      onOneSkipped.check(After + 20)
      iterator.skipToEventAfter(After + 45)(onOneSkipped) shouldEqual false
      onOneSkipped.check(After + 40)
      assert(!iterator.hasNext)

      iterator.seek(firstPos)
      iterator.skipToEventAfter(After)(onOneSkipped) shouldEqual true
      onOneSkipped.check()
      assert(iterator.next() == TestEvents(0))

      iterator.seek(firstPos)
      iterator.skipToEventAfter(After + 10)(onOneSkipped) shouldEqual true
      onOneSkipped.check(After + 10)
      assert(iterator.next() == TestEvents(1))

      iterator.seek(firstPos)
      iterator.skipToEventAfter(After + 40)(onOneSkipped) shouldEqual true
      onOneSkipped.check(After + 40)
      assert(!iterator.hasNext)

      iterator.seek(firstPos)
      val a = iterator.positionAndEventId
      assert(a.value == After)
      assert(iterator.next() == TestEvents(0))

      val b = iterator.positionAndEventId
      assert(b.value == TestEvents(0).eventId)
      assert(iterator.next() == TestEvents(1))

      val c = iterator.positionAndEventId
      assert(c.value == TestEvents(1).eventId)
      assert(iterator.next() == TestEvents(2))

      val d = iterator.positionAndEventId
      assert(d.value == TestEvents(2).eventId)
      assert(iterator.next() == TestEvents(3))

      assert(!iterator.hasNext)

      // Seek in transaction
      iterator.seek(a)
      assert(iterator.positionAndEventId == a)
      assert(iterator.next() == TestEvents(0))
      assert(iterator.positionAndEventId == b)
      assert(iterator.next() == TestEvents(1))
      assert(iterator.positionAndEventId == c)
      assert(iterator.next() == TestEvents(2))
      assert(iterator.positionAndEventId == d)
      assert(iterator.next() == TestEvents(3))

      iterator.seek(b)
      assert(iterator.positionAndEventId == b)
      assert(iterator.next() == TestEvents(1))
      assert(iterator.positionAndEventId == c)
      assert(iterator.next() == TestEvents(2))
      assert(iterator.positionAndEventId == d)
      assert(iterator.next() == TestEvents(3))

      iterator.seek(c)
      assert(iterator.positionAndEventId == c)
      assert(iterator.next() == TestEvents(2))
      val commitOffset = 2 + JournalHeaders.Commit.asJson.compactPrint.length  // Positions before and after Commit are equivalent (Commit is ignored)
      assert(iterator.positionAndEventId.copy(position = iterator.positionAndEventId.position + commitOffset) == d)
      assert(iterator.next() == TestEvents(3))

      iterator.seek(d)
      assert(iterator.positionAndEventId == d)
      assert(iterator.next() == TestEvents(3))

      iterator.close()
    }
  }
}

object FileEventIteratorTest {
  private val After = EventId(1000)
  private val TestEvents =
    Stamped(After + 10, "A" <-: AEvent) ::
    Stamped(After + 20, "B" <-: AEvent) ::  // Transaction
    Stamped(After + 30, "C" <-: AEvent) ::  // Transaction
    Stamped(After + 40, "D" <-: AEvent) ::
    Nil
}
