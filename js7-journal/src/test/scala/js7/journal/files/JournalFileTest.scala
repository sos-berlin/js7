package js7.journal.files

import java.nio.file.Files.{createTempFile, delete, size}
import java.nio.file.{Path, Paths}
import js7.base.io.file.FileUtils.syntax._
import js7.journal.files.JournalFile.garbagePattern
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

  "garbagePattern" in {
    assert(!garbagePattern(Paths.get("A")).matcher("A--0.journal").matches)
    assert(garbagePattern(Paths.get("A")).matcher("A--0.journal.tmp").matches)
    //assert(garbagePattern(Paths.get("A")).matcher("A--0.journal~").matches)
    //assert(garbagePattern(Paths.get("A")).matcher("A--0.journal~XX").matches)

    assert(!garbagePattern(Paths.get("A")).matcher("A--123456789.journal").matches)
    assert(garbagePattern(Paths.get("A")).matcher("A--123456789.journal.tmp").matches)
    //assert(garbagePattern(Paths.get("A")).matcher("A--123456789.journal~").matches)
    //assert(garbagePattern(Paths.get("A")).matcher("A--123456789.journal~XX").matches)
  }
}
