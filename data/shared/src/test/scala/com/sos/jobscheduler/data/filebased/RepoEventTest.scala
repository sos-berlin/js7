package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec
import com.sos.jobscheduler.data.crypt.{GenericSignature, SignedString}
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, VersionAdded}
import com.sos.jobscheduler.data.filebased.RepoEventTest._
import com.sos.jobscheduler.data.master.MasterFileBaseds._
import com.sos.jobscheduler.data.workflow.instructions.Fail
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.syntax.EncoderOps
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

    val workflow = Workflow(WorkflowPath("/WORKFLOW"), Vector(Fail(None)))

    "FileBasedAdded" in {
      testJson[RepoEvent](
        FileBasedAdded(workflow.path, SignedString((workflow: FileBased).asJson.compactPrint, GenericSignature("PGP", "SIGNATURE"))),
        json"""{
          "TYPE": "FileBasedAdded",
          "path": "Workflow:/WORKFLOW",
          "signed": {
            "string": "{\"TYPE\":\"Workflow\",\"path\":\"/WORKFLOW\",\"instructions\":[{\"TYPE\":\"Fail\"}]}",
            "signature": {
              "TYPE": "PGP",
              "signatureString": "SIGNATURE"
            }
          }
        }""")
    }

    "FileBasedChanged" in {
      testJson[RepoEvent](
        FileBasedChanged(workflow.path, SignedString((workflow: FileBased).asJson.compactPrint, GenericSignature("PGP", "SIGNATURE"))),
        json"""{
          "TYPE": "FileBasedChanged",
          "path": "Workflow:/WORKFLOW",
          "signed": {
            "string": "{\"TYPE\":\"Workflow\",\"path\":\"/WORKFLOW\",\"instructions\":[{\"TYPE\":\"Fail\"}]}",
            "signature": {
              "TYPE": "PGP",
              "signatureString": "SIGNATURE"
            }
          }
        }""")
    }

    "FileBasedDeleted" in {
      testJson[RepoEvent](
        FileBasedDeleted(WorkflowPath("/TEST")),
        json"""{
          "TYPE": "FileBasedDeleted",
          "path": "Workflow:/TEST"
        }""")
    }
  }

  //"FileBasedAdded must have a non-anonymous path but not a versionId" in {
  //  intercept[RuntimeException] { FileBasedAdded(Workflow.of(Fail(None))) }
  //  intercept[RuntimeException] { FileBasedAdded(Workflow(WorkflowPath("/A") % "VERSION", Vector(Fail(None)))) }
  //}
  //
  //"FileBasedChanged must have a non-anonymous path but not a versionId" in {
  //  intercept[RuntimeException] { FileBasedChanged(Workflow.of(Fail(None))) }
  //  intercept[RuntimeException] { FileBasedChanged(Workflow(WorkflowPath("/A") % "VERSION", Vector(Fail(None)))) }
  //}

  "FileBasedDeleted must have a non-anonymous path" in {
    intercept[RuntimeException] { FileBasedDeleted(WorkflowPath.Anonymous) }
  }
}

object RepoEventTest
{
  private[RepoEventTest] implicit val fileBasedEventJsonCodec: TypedJsonCodec[RepoEvent] = RepoEvent.jsonCodec
}
