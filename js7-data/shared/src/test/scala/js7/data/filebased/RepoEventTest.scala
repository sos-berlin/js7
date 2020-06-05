package js7.data.filebased

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.crypt.{GenericSignature, SignedString}
import js7.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, VersionAdded}
import js7.data.filebased.RepoEventTest._
import js7.data.master.MasterFileBaseds._
import js7.data.workflow.instructions.Fail
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RepoEventTest extends AnyFreeSpec {

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
