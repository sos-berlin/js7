package js7.core.event.journal.files

import js7.common.scalautil.FileUtils.syntax._
import java.nio.file.Files.{createTempFile, delete, size}
import java.nio.file.Paths
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
}
