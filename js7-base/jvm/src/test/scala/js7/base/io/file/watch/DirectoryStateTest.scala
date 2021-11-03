package js7.base.io.file.watch

import java.nio.file.Paths
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted}
import js7.base.io.file.watch.DirectoryState.Entry
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class DirectoryStateTest extends AnyFreeSpec
{
  coupleScribeWithSlf4j()

  "JSON" in {
    testJson(DirectoryState.fromIterable(Seq(Entry(Paths.get("FILE-1")), Entry(Paths.get("FILE-2")))),
      json"""{
        "FILE-1": {},
        "FILE-2": {}
      }""")
    // In a future version, these empty braces may be filled
  }

  "readDirectory" in {
    withTemporaryDirectory("DirectoryStateTest-") { dir =>
      assert(DirectoryState.readDirectory(dir).isEmpty)
      dir / "TEST-1" := ""
      dir / "IGNORE" := ""
      dir / "TEST-2" := ""
      assert(DirectoryState.readDirectory(dir, _.toString startsWith "TEST-") ==
        DirectoryState.fromIterable(Seq(
          Entry(Paths.get("TEST-1")),
          Entry(Paths.get("TEST-2")))))
    }
  }

  "applyAndReduceEvents" in {
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
  }
}
