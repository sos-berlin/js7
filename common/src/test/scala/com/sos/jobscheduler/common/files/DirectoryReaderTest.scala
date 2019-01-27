package com.sos.jobscheduler.common.files

import com.google.common.io.MoreFiles.touch
import com.sos.jobscheduler.common.files.DirectoryReader.Entry
import com.sos.jobscheduler.common.files.DirectoryReaderTest._
import com.sos.jobscheduler.common.scalautil.FileUtils._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch.measureTime
import java.nio.file.Files.createDirectory
import java.nio.file.Paths
import java.time.Instant
import java.time.Instant.now
import java.util.Comparator
import org.scalatest.FreeSpec
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class DirectoryReaderTest extends FreeSpec
{
  "entries" in {
    withTemporaryDirectory("Z-") { dir ⇒
      touch(dir / "C")
      touch(dir / "A")
      touch(dir / "B")
      val subdir = dir / "DIR"
      createDirectory(subdir)
      touch(subdir / "A")
      touch(subdir / "B")

      val entries = DirectoryReader.entries(dir)
      assert(entries.map(_.path) ==
        Vector(
          dir / "A",
          dir / "B",
          dir / "C",
          subdir / "A",
          subdir / "B"))

      def isValidInstant(instant: Instant) = instant > now - 30.s && instant <= now
      assert(entries.forall(e ⇒ e.attributes.size == 0 && !e.attributes.isDirectory && e.attributes.isRegularFile &&
        isValidInstant(e.attributes.lastModifiedTime.toInstant)))

      // Second call yields equivalent result
      assert(entries == DirectoryReader.entries(dir))
    }
  }

  if (sys.props contains "test.speed") "Sort speed" - {
    val n = 200000
    val entries = 1 to n map (_ ⇒ Entry(Paths.get(Random.nextString(100)), null))

    "single thread" in {
      for (_ ← 1 to 10) {
        logger.info(
          measureTime(1, "directories", warmUp = 0) {
            entries.sortBy(_.path)
          }.toString)
      }
    }

    "parallel" in {
      for (_ ← 1 to 10) {
        val comparator: Comparator[Entry] = (a, b) ⇒ a.path compareTo b.path
        logger.info(
          measureTime(1, "directories", warmUp = 0) {
            val array = entries.toArray
            java.util.Arrays.parallelSort(array, comparator)
            array.toVector
          }.toString)
      }
    }
  }
}

object DirectoryReaderTest {
  private val logger = Logger(getClass)
}
