package js7.common.files

import com.google.common.io.MoreFiles.touch
import java.nio.file.Files.delete
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.withTemporaryDirectory
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class PathSeqDifferTest extends AnyFreeSpec
{
  "diff" in {
    withTemporaryDirectory("Z-") { dir =>
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
