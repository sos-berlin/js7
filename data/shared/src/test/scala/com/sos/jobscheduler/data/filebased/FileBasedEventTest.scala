package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.filebased.FileBasedEventTest._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileBasedEventTest extends FreeSpec {

  "JSON" in {
    testJson[FileBasedEvent](FileBasedEvent.FileBasedAdded(TestFileBased(TestPath("/TEST"), "CONTENT")),
      json"""{
        "TYPE": "FileBasedAdded",
        "fileBased": {
          "TYPE": "TestFileBased",
          "path": "/TEST",
          "content": "CONTENT"
        }
      }""")

    testJson[FileBasedEvent](FileBasedEvent.FileBasedChanged(TestFileBased(TestPath("/TEST"), "CONTENT")),
      json"""{
        "TYPE": "FileBasedChanged",
        "fileBased": {
          "TYPE": "TestFileBased",
          "path": "/TEST",
          "content": "CONTENT"
        }
      }""")

    testJson[FileBasedEvent](FileBasedEvent.FileBasedDeleted(TestPath("/TEST")),
      json"""{
        "TYPE": "FileBasedDeleted",
        "path": "Test:/TEST"
      }""")
  }
}

object FileBasedEventTest {
  final case class TestPath(string: String) extends TypedPath {
    def companion = TestPath
  }
  object TestPath extends TypedPath.Companion[TestPath] {
    def sourceTypeToFilenameExtension = Map.empty
  }

  final case class TestFileBased(path: TestPath, content: String) extends FileBased

  private implicit val fileBasedJsonCodec: TypedJsonCodec[FileBased] = TypedJsonCodec(
    Subtype(deriveCodec[TestFileBased]))

  private implicit val typedPathCodec: CirceCodec[TypedPath] = TypedPath.jsonCodec(List(TestPath))
  implicit val fileBasedEventJsonCodec: TypedJsonCodec[FileBasedEvent] = FileBasedEvent.jsonCodec
}
