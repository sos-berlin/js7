package js7.journal.files

import java.nio.file.Files.{createTempFile, delete, size}
import java.nio.file.Paths
import js7.base.io.file.FileUtils.syntax._
import js7.journal.files.JournalFile.{anyJournalFilePattern, garbagePattern}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalFileTest extends AnyFreeSpec
{
  "properLength" in {
    val file = createTempFile("JournalFileTest-", ".tmp")
    assert(JournalFile(0, file).properLength == 0)

    file := "PROPER\nTWO\n"
    assert(JournalFile(0, file).properLength == size(file))

    file := "PROPER\nTWO"
    assert(JournalFile(0, file).properLength == 7)

    file := "EMPTY"
    assert(JournalFile(0, file).properLength == 0)

    delete(file)
  }

  "toFile" in {
    assert(JournalFile.toFile(Paths.get("DIR/NAME"), 123) == Paths.get("DIR/NAME--123.journal"))
  }

  "maybeJournalfile" in {
    val matcher = new JournalFile.Matcher(Paths.get("NAME"))
    assert(matcher.checkedEventId(Paths.get("NAME--0.journal")) == Right(0))
    assert(matcher.checkedEventId(Paths.get("NAME--1112223334445556667.journal")) == Right(1112223334445556667L))
    assert(matcher.checkedEventId(Paths.get("NAME---1_journal")).isLeft)
    assert(matcher.checkedEventId(Paths.get("NAME--0_journal")).isLeft)
    assert(matcher.checkedEventId(Paths.get("NAME--X.journal")).isLeft)
    assert(matcher.checkedEventId(Paths.get("NAME--.journal")).isLeft)
    assert(matcher.checkedEventId(Paths.get("OTHER--0.journal")).isLeft)
    assert(matcher.checkedEventId(Paths.get("--0.journal")).isLeft)
  }

  "anyJournalFilePattern" in {
    val pattern = anyJournalFilePattern(Paths.get("NAME"))
    assert(pattern.matcher("NAME-journal").matches)
    assert(pattern.matcher("NAME--0.journal").matches)
    assert(pattern.matcher("NAME--1000.journal").matches)
    assert(pattern.matcher("NAME--1000.journal~").matches)
    assert(pattern.matcher("NAME--1000.journal~GARBAGE").matches)
    assert(pattern.matcher("NAME--1000.journal.tmp").matches)
    assert(!pattern.matcher("NAME--1000.journal.gz").matches)
  }

  "garbagePattern" in {
    val pattern = garbagePattern(Paths.get("NAME"))
    assert(!pattern.matcher("NAME--0.journal").matches)
    assert(pattern.matcher("NAME--0.journal.tmp").matches)
    //assert(pattern.matcher("NAME--0.journal~").matches)
    //assert(pattern.matcher("NAME--0.journal~XX").matches)

    assert(!pattern.matcher("NAME--123456789.journal").matches)
    assert(pattern.matcher("NAME--123456789.journal.tmp").matches)
    //assert(pattern.matcher("NAME--123456789.journal~").matches)
    //assert(pattern.matcher("NAME--123456789.journal~XX").matches)
  }
}
