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

  "JSON" - {
    "VersionAdded" in {
      testJson[RepoEvent](
        RepoEvent.VersionAdded(VersionId("VERSION")),
        json"""{
          "TYPE": "VersionAdded",
          "versionId": "VERSION"
        }""")
    }

    "FileBasedAdded" in {
      testJson[RepoEvent](
        RepoEvent.FileBasedAdded(AFileBased(APath("/TEST") % "VERSION", "CONTENT")),
        json"""{
          "TYPE": "FileBasedAdded",
          "fileBased": {
            "TYPE": "AFileBased",
            "id": {
              "path": "/TEST",
              "versionId": "VERSION"
            },
            "content": "CONTENT"
          }
        }""")
    }

    "FileBasedChanged" in {
      testJson[RepoEvent](
        RepoEvent.FileBasedChanged(AFileBased(APath("/TEST")  % "VERSION", "CONTENT")),
        json"""{
          "TYPE": "FileBasedChanged",
          "fileBased": {
            "TYPE": "AFileBased",
            "id": {
              "path": "/TEST",
              "versionId": "VERSION"
            },
            "content": "CONTENT"
          }
        }""")
    }

    "FileBasedDeleted" in {
      testJson[RepoEvent](
        RepoEvent.FileBasedDeleted(APath("/TEST")),
        json"""{
          "TYPE": "FileBasedDeleted",
          "path": "A:/TEST"
        }""")
    }
  }
}

object RepoEventTest {
  private implicit val fileBasedJsonCodec: TypedJsonCodec[FileBased] = TypedJsonCodec(
    Subtype[AFileBased])

  private implicit val typedPathCodec: CirceCodec[TypedPath] = TypedPath.jsonCodec(List(APath))
  implicit val fileBasedEventJsonCodec: TypedJsonCodec[RepoEvent] = RepoEvent.jsonCodec
}
