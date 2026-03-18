package js7.base.io.file.watch

import cats.effect.IO
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted}
import js7.base.test.OurAsyncTestSuite

final class DirectoryStateTest extends OurAsyncTestSuite:

  "readDirectory" in :
    temporaryDirectoryResource[IO]("DirectoryStateTest-").use: dir =>
      for
        directoryState <- DirectoryStateJvm.readDirectory(dir)
        _ = assert(directoryState.isEmpty)
        _ = dir / "TEST-1" := ""
        _ = dir / "IGNORE" := ""
        _ = dir / "TEST-2" := ""
        directoryState <- DirectoryStateJvm.readDirectory(dir, _.toString.startsWith("TEST-"))
        _ = assert(directoryState ==
          DirectoryState(Seq(
            Path.of("TEST-1"),
            Path.of("TEST-2"))))
      yield
        succeed

  "applyAndReduceEvents" in :
    assert(DirectoryState.empty.applyAndReduceEvents(Nil) == (Nil, DirectoryState.empty))

    val (events, state) = DirectoryState.empty.applyAndReduceEvents(Seq(
      FileAdded(Path.of("1")),
      FileAdded(Path.of("2")),
      FileAdded(Path.of("3")),
      FileAdded(Path.of("3")),
      FileDeleted(Path.of("2")),
      FileDeleted(Path.of("X")),
      FileAdded(Path.of("4")),
      FileDeleted(Path.of("4")),
      FileAdded(Path.of("4"))))
    assert(events == Seq(
      FileAdded(Path.of("1")),
      FileAdded(Path.of("3")),
      FileAdded(Path.of("4"))))
    assert(state == DirectoryState(Seq(
      Path.of("1"),
      Path.of("3"),
      Path.of("4"))))
