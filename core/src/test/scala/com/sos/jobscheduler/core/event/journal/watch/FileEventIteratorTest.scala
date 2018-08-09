package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.common.scalautil.FileUtils
import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.watch.FileEventIteratorTest._
import com.sos.jobscheduler.core.event.journal.watch.TestData.{AEvent, TestEvent, TestKeyedEventJsonCodec, writeJournal}
import com.sos.jobscheduler.data.event.{EventId, Stamped}
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

      // firstEventPosition
      assert(iterator.firstEventPosition == 183)

      for (_ ← 1 to 3) {
        iterator.seek(PositionAnd(iterator.firstEventPosition, After))
        assert(iterator.next() == TestEvents(0))
        assert(iterator.next() == TestEvents(1))
        assert(!iterator.hasNext)
      }

      intercept[IllegalArgumentException] {
        iterator.seek(PositionAnd(iterator.firstEventPosition, After - 1))
      }

      iterator.seek(PositionAnd(iterator.firstEventPosition, After + 10))
      iterator.skipToEventAfter(After) shouldEqual false

      iterator.seek(PositionAnd(iterator.firstEventPosition, After))
      iterator.skipToEventAfter(After + 15) shouldEqual false
      iterator.skipToEventAfter(After + 25) shouldEqual false

      iterator.seek(PositionAnd(iterator.firstEventPosition, After))
      iterator.skipToEventAfter(After) shouldEqual true
      assert(iterator.next() == TestEvents(0))

      iterator.seek(PositionAnd(iterator.firstEventPosition, After))
      iterator.skipToEventAfter(After + 10) shouldEqual true
      assert(iterator.next() == TestEvents(1))

      iterator.seek(PositionAnd(iterator.firstEventPosition, After))
      iterator.skipToEventAfter(After + 20) shouldEqual true
      assert(!iterator.hasNext)

      iterator.close()
    }
  }
}

object FileEventIteratorTest {
  private val After = EventId(10009)
  private val TestEvents =
    Stamped(After + 10, "A" <-: AEvent) ::
    Stamped(After + 20, "B" <-: AEvent) ::
    Nil
}
