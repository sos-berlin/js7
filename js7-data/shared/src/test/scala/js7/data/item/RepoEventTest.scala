package js7.data.item

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.crypt.{GenericSignature, Signed, SignedString}
import js7.data.controller.ControllerItems._
import js7.data.item.RepoEvent.{ItemAdded, ItemChanged, ItemDeleted, VersionAdded}
import js7.data.item.RepoEventTest._
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

    "ItemAdded" in {
      testJson[RepoEvent](
        ItemAdded(Signed(workflow, SignedString.pgp((workflow: InventoryItem).asJson.compactPrint, "SIGNATURE"))),
        json"""{
          "TYPE": "ItemAdded",
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

    "ItemChanged" in {
      testJson[RepoEvent](
        ItemChanged(Signed(workflow, SignedString.pgp((workflow: InventoryItem).asJson.compactPrint, "SIGNATURE"))),
        json"""{
          "TYPE": "ItemChanged",
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

    "ItemDeleted" in {
      testJson[RepoEvent](
        ItemDeleted(WorkflowPath("/TEST")),
        json"""{
          "TYPE": "ItemDeleted",
          "path": "Workflow:/TEST"
        }""")
    }
  }

  //"ItemAdded must have a non-anonymous path but not a versionId" in {
  //  intercept[RuntimeException] { ItemAdded(Workflow.of(Fail(None))) }
  //  intercept[RuntimeException] { ItemAdded(Workflow(WorkflowPath("/A") % "VERSION", Vector(Fail(None)))) }
  //}
  //
  //"ItemChanged must have a non-anonymous path but not a versionId" in {
  //  intercept[RuntimeException] { ItemChanged(Workflow.of(Fail(None))) }
  //  intercept[RuntimeException] { ItemChanged(Workflow(WorkflowPath("/A") % "VERSION", Vector(Fail(None)))) }
  //}

  "ItemDeleted must have a non-anonymous path" in {
    intercept[RuntimeException] { ItemDeleted(WorkflowPath.Anonymous) }
  }
}

object RepoEventTest
{
  private[RepoEventTest] implicit val itemEventJsonCodec: TypedJsonCodec[RepoEvent] = RepoEvent.jsonCodec
}
