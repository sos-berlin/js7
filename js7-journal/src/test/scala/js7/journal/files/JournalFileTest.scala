package js7.journal.files

import java.nio.file.Files.{createTempFile, delete, size}
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.*
import js7.base.test.OurTestSuite
import js7.journal.files.JournalFile.{anyJournalFilePattern, garbagePattern}

/**
  * @author Joacim Zschimmer
  */
final class JournalFileTest extends OurTestSuite:

  "properLength" in:
    val file = createTempFile("JournalFileTest-", ".tmp")
    assert(JournalFile(0, file).properLength == 0)

    file := "PROPER\nTWO\n"
    assert(JournalFile(0, file).properLength == size(file))

    file := "PROPER\nTWO"
    assert(JournalFile(0, file).properLength == 7)

    file := "EMPTY"
    assert(JournalFile(0, file).properLength == 0)

    delete(file)

  "toFile" in:
    assert(JournalFile.toFile(Path.of("DIR/NAME"), 123) == Path.of("DIR/NAME--123.journal"))

  "maybeJournalfile" in:
    val matcher = new JournalFile.Matcher(Path.of("NAME"))
    assert(matcher.checkedFileEventId(Path.of("NAME--0.journal")) == Right(0))
    assert(matcher.checkedFileEventId(Path.of("NAME--1112223334445556667.journal")) == Right(1112223334445556667L))
    assert(matcher.checkedFileEventId(Path.of("NAME---1_journal")).isLeft)
    assert(matcher.checkedFileEventId(Path.of("NAME--0_journal")).isLeft)
    assert(matcher.checkedFileEventId(Path.of("NAME--X.journal")).isLeft)
    assert(matcher.checkedFileEventId(Path.of("NAME--.journal")).isLeft)
    assert(matcher.checkedFileEventId(Path.of("OTHER--0.journal")).isLeft)
    assert(matcher.checkedFileEventId(Path.of("--0.journal")).isLeft)

  "anyJournalFilePattern" in:
    val pattern = anyJournalFilePattern(Path.of("NAME"))
    assert(pattern.matcher("NAME-journal").matches)
    assert(pattern.matcher("NAME--0.journal").matches)
    assert(pattern.matcher("NAME--1000.journal").matches)
    assert(pattern.matcher("NAME--1000.journal~").matches)
    assert(pattern.matcher("NAME--1000.journal~GARBAGE").matches)
    assert(pattern.matcher("NAME--1000.journal.tmp").matches)
    assert(!pattern.matcher("NAME--1000.journal.gz").matches)

  "garbagePattern" in:
    val pattern = garbagePattern(Path.of("NAME"))
    assert(!pattern.matcher("NAME--0.journal").matches)
    assert(pattern.matcher("NAME--0.journal.tmp").matches)
    //assert(pattern.matcher("NAME--0.journal~").matches)
    //assert(pattern.matcher("NAME--0.journal~XX").matches)

    assert(!pattern.matcher("NAME--123456789.journal").matches)
    assert(pattern.matcher("NAME--123456789.journal.tmp").matches)
    //assert(pattern.matcher("NAME--123456789.journal~").matches)
    //assert(pattern.matcher("NAME--123456789.journal~XX").matches)
