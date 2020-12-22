package js7.journal.files

import com.google.common.io.MoreFiles.touch
import js7.base.problem.Problem
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.{deleteDirectoryContentRecursively, withTemporaryDirectory}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalFilesTest extends AnyFreeSpec
{
  "listJournalFiles, currentFile" in {
    withTemporaryDirectory("JournalHistoryTest-") { dir =>
      dir / "test--0.journal" := "TEST-CONTENT"
      touch(dir / "test--2000.journal")
      touch(dir / "test--1000.journal")
      touch(dir / "test-30.journal")
      touch(dir / "test.journal")
      touch(dir / "test--0.journal.tmp")
      assert(JournalFiles.listJournalFiles(dir / "test") == Vector(
        JournalFile(   0, dir / "test--0.journal"),
        JournalFile(1000, dir / "test--1000.journal"),
        JournalFile(2000, dir / "test--2000.journal")))

      assert(JournalFiles.currentFile(dir / "test") == Right(dir / "test--2000.journal"))

      deleteDirectoryContentRecursively(dir)
      assert(JournalFiles.currentFile(dir / "test") == Left(Problem(s"No journal under '${dir / "test"}'")))
    }
  }
}
