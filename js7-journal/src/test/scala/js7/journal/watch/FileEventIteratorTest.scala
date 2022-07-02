package js7.journal.watch

import io.circe.syntax.EncoderOps
import java.nio.file.Files
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.io.file.FileUtils
import js7.common.jsonseq.PositionAnd
import js7.data.event.{EventId, JournalSeparators, Stamped}
import js7.journal.data.JournalMeta
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.journal.watch.FileEventIteratorTest._
import js7.journal.watch.TestData.{AEvent, TestKeyedEventJsonCodec, journalId, writeJournal}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

/**
  * @author Joacim Zschimmer
  */
final class FileEventIteratorTest extends AnyFreeSpec
{
  "FileEventIterator" in {
    FileUtils.withTemporaryDirectory("FileEventIteratorPoolTest-") { dir =>
      val journalMeta = JournalMeta(TypedJsonCodec[Any](), TestKeyedEventJsonCodec, dir resolve "test")
      val journalFile = journalMeta.file(after = After)
      writeJournal(journalMeta, after = After, TestEvents)

      val iterator = new FileEventIterator(journalMeta, journalFile, journalId, fileEventId = After,
        () => Files.size(journalFile))
      iterator.firstEventPosition  // Must be called before reading
      iterator.next()
      val firstPos = PositionAnd(iterator.firstEventPosition, After)
      val secondPos = PositionAnd(iterator.position, TestEvents(0).eventId)

      for (_ <- 1 to 3) {
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
      val journalIndex = new JournalIndex(PositionAnd(100/*dummy*/, EventId.BeforeFirst), size = 100)
      iterator.skipToEventAfter(journalIndex, After) shouldEqual false
      assert(journalIndex.highestEventId == EventId.BeforeFirst)
      iterator.next() shouldEqual TestEvents(1)

      iterator.seek(firstPos)
      iterator.skipToEventAfter(journalIndex, After + 15) shouldEqual false
      assert(journalIndex.highestEventId == After + 20)
      iterator.skipToEventAfter(journalIndex, After + 45) shouldEqual false
      assert(journalIndex.highestEventId == After + 40)
      assert(!iterator.hasNext)

      iterator.seek(firstPos)
      iterator.skipToEventAfter(journalIndex, After) shouldEqual true
      assert(journalIndex.highestEventId == After + 40)
      assert(iterator.next() == TestEvents(0))

      iterator.seek(firstPos)
      iterator.skipToEventAfter(journalIndex, After + 10) shouldEqual true
      assert(journalIndex.highestEventId == After + 40)
      assert(iterator.next() == TestEvents(1))

      iterator.seek(firstPos)
      iterator.skipToEventAfter(journalIndex, After + 40) shouldEqual true
      assert(journalIndex.highestEventId == After + 40)
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
      // Positions before and after Commit are equivalent (Commit is ignored)
      val commitOffset = 1 + JournalSeparators.Commit.asJson.compactPrint.length
      assert(iterator.positionAndEventId.copy(
        position = iterator.positionAndEventId.position + commitOffset) == d)
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
