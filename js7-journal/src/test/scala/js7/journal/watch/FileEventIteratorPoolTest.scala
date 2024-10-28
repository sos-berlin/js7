package js7.journal.watch

import cats.effect.unsafe.IORuntime
import java.nio.file.Files
import js7.base.io.file.FileUtils
import js7.base.test.OurTestSuite
import js7.data.event.{EventId, Stamped}
import js7.journal.data.JournalLocation
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.watch.FileEventIteratorPoolTest.*
import js7.journal.watch.TestData.{AEvent, TestState, journalId, writeJournal}

/**
  * @author Joacim Zschimmer
  */
final class FileEventIteratorPoolTest extends OurTestSuite:

  private given IORuntime = ioRuntime

  "FileEventIteratorPool" in:
    FileUtils.withTemporaryDirectory("FileEventIteratorPoolTest-") { dir =>
      val journalLocation = JournalLocation(TestState, dir.resolve("test"))
      val journalFile = journalLocation.file(after = After)
      writeJournal(journalLocation, after = After, TestEvents)
      val pool = new FileEventIteratorPool(journalLocation, journalId,
        journalFile, fileEventId = After, () => Files.size(journalFile))

      assert(pool.firstEventPosition > 0)

      locally:
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
      intercept[ClosedException]:
        pool.borrowIterator()
    }


object FileEventIteratorPoolTest:
  private val After = EventId(10009)
  private val TestEvents =
    Stamped(After + 1, "A" <-: AEvent) ::
    Stamped(After + 2, "B" <-: AEvent) ::
    Nil
