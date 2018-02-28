package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.filebased.RepoEventTest._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RepoEventTest extends FreeSpec {

  "JSON" in {
    testJson[RepoEvent](
      RepoEvent.VersionAdded(FileBasedVersion("VERSION")),
      json"""{
        "TYPE": "VersionAdded",
        "version": "VERSION"
      }""")

    testJson[RepoEvent](
      RepoEvent.FileBasedAdded(TestFileBased(TestPath("/TEST"), "CONTENT")),
      json"""{
        "TYPE": "FileBasedAdded",
        "fileBased": {
          "TYPE": "TestFileBased",
          "path": "/TEST",
          "content": "CONTENT"
        }
      }""")

    testJson[RepoEvent](
      RepoEvent.FileBasedChanged(TestFileBased(TestPath("/TEST"), "CONTENT")),
      json"""{
        "TYPE": "FileBasedChanged",
        "fileBased": {
          "TYPE": "TestFileBased",
          "path": "/TEST",
          "content": "CONTENT"
        }
      }""")

    testJson[RepoEvent](
      RepoEvent.FileBasedDeleted(TestPath("/TEST")),
      json"""{
        "TYPE": "FileBasedDeleted",
        "path": "Test:/TEST"
      }""")
  }
}

object RepoEventTest {
  private implicit val fileBasedJsonCodec: TypedJsonCodec[FileBased] = TypedJsonCodec(
    Subtype[TestFileBased])

  private implicit val typedPathCodec: CirceCodec[TypedPath] = TypedPath.jsonCodec(List(TestPath))
  implicit val fileBasedEventJsonCodec: TypedJsonCodec[RepoEvent] = RepoEvent.jsonCodec
}
