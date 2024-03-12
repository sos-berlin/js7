package js7.journal.files

import java.io.FileOutputStream
import java.nio.file.Paths
import js7.base.io.file.FileUtils.implicits.pathToFile
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{deleteDirectoryContentRecursively, touchFile, withTemporaryDirectory}
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.journal.files.JournalFiles.{deleteJournal, deleteJournalIfMarked, deletionMarkerFile}

/**
  * @author Joacim Zschimmer
  */
final class JournalFilesTest extends OurTestSuite:

  "listJournalFiles, currentFile" in:
    withTemporaryDirectory("JournalHistoryTest-") { dir =>
      dir / "test--0.journal" := "TEST-CONTENT"
      touchFile(dir / "test--2000.journal")
      touchFile(dir / "test--1000.journal")
      touchFile(dir / "test-30.journal")
      touchFile(dir / "test.journal")
      touchFile(dir / "test-XX.journal")
      touchFile(dir / "test--100.journal.tmp")
      touchFile(dir / "test--1000.journal~garbage")
      assert(JournalFiles.listJournalFiles(dir / "test") == Vector(
        JournalFile(   0, dir / "test--0.journal"),
        JournalFile(1000, dir / "test--1000.journal"),
        JournalFile(2000, dir / "test--2000.journal")))

      assert(JournalFiles.currentFile(dir / "test") == Right(dir / "test--2000.journal"))

      assert(JournalFiles.listGarbageFiles(dir / "test", 0).isEmpty)
      assert(JournalFiles.listGarbageFiles(dir / "test", 100).isEmpty)
      assert(JournalFiles.listGarbageFiles(dir / "test", 101) == Vector(dir / "test--100.journal.tmp"))
      assert(JournalFiles.listGarbageFiles(dir / "test", 999) == Vector(dir / "test--100.journal.tmp"))
      assert(JournalFiles.listGarbageFiles(dir / "test", 1000) == Vector(dir / "test--100.journal.tmp"))
      assert(JournalFiles.listGarbageFiles(dir / "test", 1001) ==
        Vector(dir / "test--100.journal.tmp"/*, dir / "test--1000.journal~garbage"*/))

      deleteDirectoryContentRecursively(dir)
      assert(JournalFiles.currentFile(dir / "test") == Left(Problem(s"No journal under '${dir / "test"}'")))
      assert(JournalFiles.listGarbageFiles(dir / "test", 1001).isEmpty)
    }

  "deletionMarkerFile" in:
    assert(deletionMarkerFile(Paths.get("DIR/NAME")) == Paths.get("DIR/NAME-DELETE!"))

  "deleteJournalIfMarked" in:
    withTemporaryDirectory("JournalHistoryTest-") { dir =>
      val fileBase = dir / "test"
      val files = Set(
        dir / "test-journal",
        dir / "test--0.journal",
        dir / "test--1000.journal",
        dir / "test--1000.journal~",
        dir / "test--1000.journal~garbage",
        dir / "test--1000.journal.tmp",
        dir / "test--1000.journal.gz",
        dir / "other--0.journal")
      files foreach touchFile

      deleteJournalIfMarked(fileBase)
      assert(dir.directoryContentsAs(Set) == files)

      touchFile(deletionMarkerFile(fileBase))
      deleteJournalIfMarked(fileBase)
      assert(dir.directoryContentsAs(Set) == Set(dir / "other--0.journal", dir / "test--1000.journal.gz"))
    }

  "deleteJournal, then deleteJournalIfMarked" in:
    withTemporaryDirectory("JournalHistoryTest-") { dir =>
      val fileBase = dir / "test"
      val files = Set(
        dir / "test-journal",
        dir / "test--0.journal",
        dir / "test--1000.journal",
        dir / "test--1000.journal~",
        dir / "test--1000.journal~garbage",
        dir / "test--1000.journal.tmp",
        dir / "test--1000.journal.gz",
        dir / "other--0.journal")

      val openedFiles = for file <- files yield
        new FileOutputStream(file.toPath)  // Make file undeletable under Windows

      // Create the marker file before the directory is no longer writable
      touchFile(deletionMarkerFile(fileBase))
      dir.toFile.setWritable(false) // Make files undeletable under Unix
      deleteJournal(fileBase, ignoreFailure = true)
      assert(dir.directoryContentsAs(Set) == files + deletionMarkerFile(fileBase))

      dir.toFile.setWritable(true)
      for file <- openedFiles do file.close()
      deleteJournalIfMarked(fileBase)
      assert(dir.directoryContentsAs(Set) == Set(dir / "other--0.journal", dir / "test--1000.journal.gz"))
    }
