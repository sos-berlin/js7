package js7.cluster

import java.nio.file.Files.{delete, exists, size}
import java.nio.file.Paths
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{withTemporaryDirectory, withTemporaryFile}
import js7.base.test.OurTestSuite
import js7.data.event.JournalPosition

final class ClusterTest extends OurTestSuite
{
  "truncateFile" in {
    withTemporaryFile("ClusterCommonTest", ".tmp") { file =>
      val a = "CONTENT\n"
      val b = (1 to 4906).map(_.toString).mkString(",")
      file := a + b
      assert(size(file) > a.length)

      Cluster.truncateFile(file, a.length)
      assert(size(file) == a.length)
      assert(file.contentString == a)
      val truncated = Paths.get(file.toString + "~TRUNCATED-AFTER-FAILOVER")
      assert(truncated.contentString == b)

      delete(truncated)
    }
  }

  "truncateJournal" in {
    withTemporaryDirectory("ClusterTest-") { dir =>
      val fileBase = dir / "TEST"
      val a = Paths.get(s"$fileBase--1000.journal")
      val b = Paths.get(s"$fileBase--2000.journal")
      val c = Paths.get(s"$fileBase--3000.journal")
      a := "ONE\nTWO\n"
      b := "THREE\nFOUR\n"
      c := "FIVE\nSIX\n"

      intercept[RuntimeException] {
        Cluster.truncateJournal(fileBase, JournalPosition(3333, 0))
      }
      intercept[RuntimeException] {
        Cluster.truncateJournal(fileBase, JournalPosition(3000, 7))
      }

      var maybeFile = Cluster.truncateJournal(fileBase, JournalPosition(3000, 9))
      assert(maybeFile.isEmpty)
      assert(a.contentString == "ONE\nTWO\n")
      assert(b.contentString == "THREE\nFOUR\n")
      assert(c.contentString == "FIVE\nSIX\n")

      intercept[RuntimeException] {
        Cluster.truncateJournal(fileBase, JournalPosition(3000, 7))
      }

      maybeFile = Cluster.truncateJournal(fileBase, JournalPosition(3000, 5))
      assert(maybeFile == Some(c))
      assert(a.contentString == "ONE\nTWO\n")
      assert(b.contentString == "THREE\nFOUR\n")
      assert(c.contentString == "FIVE\n")

      maybeFile = Cluster.truncateJournal(fileBase, JournalPosition(2000, 6))
      assert(maybeFile == Some(b))
      assert(a.contentString == "ONE\nTWO\n")
      assert(b.contentString == "THREE\n")
      assert(!exists(c))
    }
  }
}
