package js7.base.io.file.watch

import java.nio.file.Paths
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{touchFile, withTemporaryDirectory}
import js7.base.io.file.watch.DirectoryStateJvm.readDirectory
import js7.base.test.OurTestSuite

final class DirectoryStateJvmTest extends OurTestSuite:

  "readDirectory" in:
    withTemporaryDirectory("DirectoryStateJvmTest-") { dir =>
      assert(readDirectory(dir).isEmpty)

      touchFile(dir / "1")
      touchFile(dir / "TEST")

      locally:
        val directoryState = readDirectory(dir)
        assert(!directoryState.isEmpty)
        assert(directoryState.files.to(Set) == Set(Paths.get("1"), Paths.get("TEST")))

      locally:
        val directoryState = readDirectory(dir, _.startsWith("1"))
        assert(!directoryState.isEmpty)
        assert(directoryState.files == Set(Paths.get("1")))
    }
