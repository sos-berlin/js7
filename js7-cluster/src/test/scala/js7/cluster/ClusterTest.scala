package js7.cluster

import java.nio.file.Files.exists
import java.nio.file.Paths
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.data.event.JournalPosition
import org.scalatest.freespec.AnyFreeSpec

final class ClusterTest extends AnyFreeSpec
{
  "truncateFile" in {
    withTemporaryDirectory("ClusterTest-") { dir =>
      val fileBase = dir / "TEST"
      val a = Paths.get(s"$fileBase--1000.journal")
      val b = Paths.get(s"$fileBase--2000.journal")
      val c = Paths.get(s"$fileBase--3000.journal")
      a := "ONE\nTWO\n"
      b := "THREE\nFOUR\n"
      c := "FIVE\nSIX\n"

      intercept[RuntimeException] {
        Cluster.truncateJournal(fileBase, JournalPosition(3333, 0), false)
      }
      intercept[RuntimeException] {
        Cluster.truncateJournal(fileBase, JournalPosition(3000, 7), false)
      }

      var maybeFile = Cluster.truncateJournal(fileBase, JournalPosition(3000, 9), false)
      assert(maybeFile.isEmpty)
      assert(a.contentString == "ONE\nTWO\n")
      assert(b.contentString == "THREE\nFOUR\n")
      assert(c.contentString == "FIVE\nSIX\n")

      intercept[RuntimeException] {
        Cluster.truncateJournal(fileBase, JournalPosition(3000, 7), false)
      }

      maybeFile = Cluster.truncateJournal(fileBase, JournalPosition(3000, 5), false)
      assert(maybeFile == Some(c))
      assert(a.contentString == "ONE\nTWO\n")
      assert(b.contentString == "THREE\nFOUR\n")
      assert(c.contentString == "FIVE\n")

      maybeFile = Cluster.truncateJournal(fileBase, JournalPosition(2000, 6), false)
      assert(maybeFile == Some(b))
      assert(a.contentString == "ONE\nTWO\n")
      assert(b.contentString == "THREE\n")
      assert(!exists(c))
    }
  }
}
