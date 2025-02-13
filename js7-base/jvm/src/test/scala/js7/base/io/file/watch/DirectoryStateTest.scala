package js7.base.io.file.watch

import cats.effect.IO
import java.nio.file.Paths
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted}
import js7.base.io.file.watch.DirectoryState.Entry
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
          DirectoryState.fromIterable(Seq(
            Entry(Paths.get("TEST-1")),
            Entry(Paths.get("TEST-2")))))
      yield
        succeed

  "applyAndReduceEvents" in :
    assert(DirectoryState.empty.applyAndReduceEvents(Nil) == (Nil, DirectoryState.empty))

    var (events, state) = DirectoryState.empty.applyAndReduceEvents(Seq(
      FileAdded(Paths.get("1")),
      FileAdded(Paths.get("2")),
      FileAdded(Paths.get("3")),
      FileAdded(Paths.get("3")),
      FileDeleted(Paths.get("2")),
      FileDeleted(Paths.get("X")),
      FileAdded(Paths.get("4")),
      FileDeleted(Paths.get("4")),
      FileAdded(Paths.get("4"))))
    assert(events == Seq(
      FileAdded(Paths.get("1")),
      FileAdded(Paths.get("3")),
      FileAdded(Paths.get("4"))))
    assert(state == DirectoryState.fromIterable(Seq(
      Entry(Paths.get("1")),
      Entry(Paths.get("3")),
      Entry(Paths.get("4")))))
