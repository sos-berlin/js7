package com.sos.jobscheduler.core.event.journal.files

import cats.data.Validated.{Invalid, Valid}
import com.google.common.io.Files.touch
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.{deleteDirectoryContentRecursively, withTemporaryDirectory}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalFilesTest extends FreeSpec
{
  "listJournalFiles, currentFile" in {
    withTemporaryDirectory("JournalHistoryTest-") { dir =>
      (dir / "test--0.journal").contentString = "TEST-CONTENT"
      touch(dir / "test--2000.journal")
      touch(dir / "test--1000.journal")
      touch(dir / "test-30.journal")
      touch(dir / "test.journal")
      touch(dir / "test--0.journal.tmp")
      assert(JournalFiles.listJournalFiles(dir / "test") == Vector(
        JournalFile(   0, dir / "test--0.journal"),
        JournalFile(1000, dir / "test--1000.journal"),
        JournalFile(2000, dir / "test--2000.journal")))

      assert(JournalFiles.currentFile(dir / "test") == Valid(dir / "test--2000.journal"))

      deleteDirectoryContentRecursively(dir)
      assert(JournalFiles.currentFile(dir / "test") == Invalid(Problem(s"No journal under '${dir / "test"}'")))
    }
  }
}
