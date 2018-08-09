package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.common.scalautil.FileUtils
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.watch.FileEventIteratorTest._
import com.sos.jobscheduler.core.event.journal.watch.TestData.{AEvent, TestEvent, TestKeyedEventJsonCodec, writeJournal}
import com.sos.jobscheduler.data.event.{EventId, Stamped}
import java.nio.file.Files
import org.scalatest.FreeSpec

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
        iterator.seek(iterator.firstEventPosition)
        assert(iterator.next() == TestEvents(0))
        assert(iterator.next() == TestEvents(1))
        assert(!iterator.hasNext)
      }

      iterator.close()
    }
  }
}

object FileEventIteratorTest {
  private val After = EventId(10009)
  private val TestEvents =
    Stamped(After + 1, "A" <-: AEvent) ::
    Stamped(After + 2, "B" <-: AEvent) ::
    Nil
}
