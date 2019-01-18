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
        VersionAdded(VersionId("VERSION")),
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
        FileBasedDeleted(APath("/TEST")),
        json"""{
          "TYPE": "FileBasedDeleted",
          "path": "A:/TEST"
        }""")
    }
  }

  "FileBasedAdded must have a non-anonymous path but not a versionId" in {
    intercept[RuntimeException] { FileBasedAdded(Workflow.of(Fail(None))) }
    intercept[RuntimeException] { FileBasedAdded(Workflow(WorkflowPath("/A") % "VERSION", Vector(Fail(None)))) }
  }

  "FileBasedChanged must have a non-anonymous path but not a versionId" in {
    intercept[RuntimeException] { FileBasedChanged(Workflow.of(Fail(None))) }
    intercept[RuntimeException] { FileBasedChanged(Workflow(WorkflowPath("/A") % "VERSION", Vector(Fail(None)))) }
  }

  "FileBasedDeleted must have a non-anonymous path" in {
    intercept[RuntimeException] { FileBasedDeleted(WorkflowPath.Anonymous) }
  }
}

object RepoEventTest {
  private implicit val fileBasedJsonCodec: TypedJsonCodec[FileBased] = TypedJsonCodec(
    Subtype[Workflow])

  private implicit val typedPathCodec: CirceCodec[TypedPath] = TypedPath.jsonCodec(List(APath))
  implicit val fileBasedEventJsonCodec: TypedJsonCodec[RepoEvent] = RepoEvent.jsonCodec
}
