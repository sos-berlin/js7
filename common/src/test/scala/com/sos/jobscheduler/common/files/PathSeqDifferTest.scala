package com.sos.jobscheduler.common.files

import com.google.common.io.MoreFiles.touch
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryDirectory
import java.nio.file.Files.delete
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class PathSeqDifferTest extends FreeSpec
{
  "diff" in {
    withTemporaryDirectory("Z-") { dir ⇒
      touch(dir / "A")
      val entries1 = DirectoryReader.entries(dir)
      assert(PathSeqDiffer.diff(entries1, DirectoryReader.entries(dir)) == PathSeqDiff(Nil, Nil, Nil))

      touch(dir / "B")
      val entries2 = DirectoryReader.entries(dir)
      assert(PathSeqDiffer.diff(entries2, entries1) == PathSeqDiff(added = dir / "B" :: Nil))

      delete(dir / "A")
      val entries3 = DirectoryReader.entries(dir)
      assert(PathSeqDiffer.diff(entries3, entries2) == PathSeqDiff(deleted = dir / "A" :: Nil))

      dir / "B" ++= "CHANGED"
      val entries4 = DirectoryReader.entries(dir)
      assert(PathSeqDiffer.diff(entries4, entries2) == PathSeqDiff(changed = dir / "B" :: Nil, deleted = dir / "A" :: Nil))
    }
  }
}
