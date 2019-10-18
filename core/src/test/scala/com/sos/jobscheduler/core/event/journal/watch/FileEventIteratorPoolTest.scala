package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.common.scalautil.FileUtils
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.JournalMetaOps
import com.sos.jobscheduler.core.event.journal.watch.FileEventIteratorPoolTest._
import com.sos.jobscheduler.core.event.journal.watch.TestData.{AEvent, TestKeyedEventJsonCodec, journalId, writeJournal}
import com.sos.jobscheduler.data.event.{EventId, Stamped}
import java.nio.file.Files
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileEventIteratorPoolTest extends FreeSpec
{
  "FileEventIteratorPool" in {
    FileUtils.withTemporaryDirectory("FileEventIteratorPoolTest-") { dir =>
      val journalMeta = JournalMeta(TypedJsonCodec[Any](), TestKeyedEventJsonCodec, dir resolve "test")
      val journalFile = journalMeta.file(after = After)
      writeJournal(journalMeta, after = After, TestEvents)
      val pool = new FileEventIteratorPool(journalMeta, Some(journalId),
        journalFile, tornEventId = After, () => Files.size(journalFile))

      assert(pool.firstEventPosition > 0)

      locally {
        // borrowIterator, returnIterator
        val it1 = pool.borrowIterator()
        assert(it1.eventId == After)
        val it2 = pool.borrowIterator()
        val it3 = pool.borrowIterator()
        assert(pool.freeIteratorsCount == 0 && pool.lentIteratorsCount == 3)
        pool.returnIterator(it2)
        pool.returnIterator(it3)
        assert(pool.freeIteratorsCount == 2 && pool.lentIteratorsCount == 1)

        // borrowIterator returns last freed iterator (maybe better memory cache usage)
        val it4 = pool.borrowIterator()
        assert(it4 eq it3)
        val it5 = pool.borrowIterator()
        assert(it5 eq it2)
        pool.returnIterator(it4)
        pool.returnIterator(it4)
        pool.returnIterator(it1)
      }

      val it1 = pool.borrowIterator()
      assert(it1.position == it1.firstEventPosition)  // it1 is the first opened iterator, used for firstEventPosition
      assert(it1.eventId == After)
      assert(it1.next() == TestEvents(0))
      pool.returnIterator(it1)

      val it1a = pool.borrowIterator()
      assert(it1a eq it1)
      assert(it1a.position - it1a.firstEventPosition == 44)
      assert(it1a.eventId == After + 1)
      assert(it1a.next() == TestEvents(1))
      assert(!it1a.hasNext)

      pool.close()  // Closes all iterators (and files)
      intercept[ClosedException] {
        pool.borrowIterator()
      }
    }
  }
}

object FileEventIteratorPoolTest {
  private val After = EventId(10009)
  private val TestEvents =
    Stamped(After + 1, "A" <-: AEvent) ::
    Stamped(After + 2, "B" <-: AEvent) ::
    Nil
}
