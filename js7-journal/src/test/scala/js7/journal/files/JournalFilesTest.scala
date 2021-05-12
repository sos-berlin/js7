package js7.journal.files

import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.{deleteDirectoryContentRecursively, touchFile, withTemporaryDirectory}
import js7.base.problem.Problem
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalFilesTest extends AnyFreeSpec
{
  "listJournalFiles, currentFile" in {
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
  }
}
