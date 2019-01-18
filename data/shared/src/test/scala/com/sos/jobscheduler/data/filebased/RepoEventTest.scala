package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, VersionAdded}
import com.sos.jobscheduler.data.filebased.RepoEventTest._
import com.sos.jobscheduler.data.workflow.instructions.Fail
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
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
        FileBasedAdded(Workflow(WorkflowPath("/WORKFLOW"), Vector(Fail(None)))),
        json"""{
          "TYPE": "FileBasedAdded",
          "fileBased": {
            "TYPE": "Workflow",
            "path": "/WORKFLOW",
            "instructions": [
              {
                "TYPE": "Fail"
              }
            ]
          }
        }""")
    }

    "FileBasedChanged" in {
      testJson[RepoEvent](
        FileBasedChanged(Workflow(WorkflowPath("/WORKFLOW"), Vector(Fail(None)))),
        json"""{
          "TYPE": "FileBasedChanged",
          "fileBased": {
            "TYPE": "Workflow",
            "path": "/WORKFLOW",
            "instructions": [
              {
                "TYPE": "Fail"
              }
            ]
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
    Subtype[Workflow])

  private implicit val typedPathCodec: CirceCodec[TypedPath] = TypedPath.jsonCodec(List(APath))
  implicit val fileBasedEventJsonCodec: TypedJsonCodec[RepoEvent] = RepoEvent.jsonCodec
}
