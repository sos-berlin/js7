package js7.base.io.file.watch

import cats.effect.IO
import java.nio.file.Paths
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{temporaryDirectoryResource, touchFile}
import js7.base.io.file.watch.DirectoryStateJvm.readDirectory
import js7.base.test.OurAsyncTestSuite

final class DirectoryStateJvmTest extends OurAsyncTestSuite:

  "readDirectory" in:
    temporaryDirectoryResource[IO]("DirectoryStateJvmTest-").use: dir =>
      for
        directoryState <- readDirectory(dir)
        _ = assert(directoryState.isEmpty)
        _ = touchFile(dir / "1")
        _ = touchFile(dir / "TEST")
        directoryState <- readDirectory(dir)
        _ = assert(!directoryState.isEmpty)
        _ = assert(directoryState.files.to(Set) == Set(Paths.get("1"), Paths.get("TEST")))
        directoryState <- readDirectory(dir, _.startsWith("1"))
        _ = assert(!directoryState.isEmpty)
        _ = assert(directoryState.files == Set(Paths.get("1")))
      yield
        succeed
