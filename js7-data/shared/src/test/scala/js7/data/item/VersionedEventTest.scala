package js7.data.item

import io.circe.Codec
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.{Signed, SignedString}
import js7.base.test.OurTestSuite
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded, VersionedItemChanged, VersionedItemRemoved}
import js7.data.workflow.instructions.Fail
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class VersionedEventTest extends OurTestSuite
{
  import VersionedEventTest.{itemEventJsonCodec, itemJsonCodec}

  "JSON" - {
    "VersionAdded" in {
      testJson[VersionedEvent](
        VersionAdded(VersionId("VERSION")),
        json"""{
          "TYPE": "VersionAdded",
          "versionId": "VERSION"
        }""")
    }

    val workflow = Workflow(WorkflowPath("WORKFLOW"), Vector(Fail(None)))

    "VersionedItemAdded" in {
      testJson[VersionedEvent](
        VersionedItemAdded(Signed(workflow, SignedString.pgp((workflow: VersionedItem).asJson.compactPrint, "SIGNATURE"))),
        json"""{
          "TYPE": "VersionedItemAdded",
          "signed": {
            "string": "{\"TYPE\":\"Workflow\",\"path\":\"WORKFLOW\",\"instructions\":[{\"TYPE\":\"Fail\"}]}",
            "signature": {
              "TYPE": "PGP",
              "signatureString": "SIGNATURE"
            }
          }
        }""")
    }

    "VersionedItemChanged" in {
      testJson[VersionedEvent](
        VersionedItemChanged(Signed(workflow, SignedString.pgp((workflow: VersionedItem).asJson.compactPrint, "SIGNATURE"))),
        json"""{
          "TYPE": "VersionedItemChanged",
          "signed": {
            "string": "{\"TYPE\":\"Workflow\",\"path\":\"WORKFLOW\",\"instructions\":[{\"TYPE\":\"Fail\"}]}",
            "signature": {
              "TYPE": "PGP",
              "signatureString": "SIGNATURE"
            }
          }
        }""")
    }

    "VersionedItemRemoved" in {
      testJson[VersionedEvent](
        VersionedItemRemoved(WorkflowPath("TEST")),
        json"""{
          "TYPE": "VersionedItemRemoved",
          "path": "Workflow:TEST"
        }""")
    }
  }

  //"VersionedItemAdded must have a non-anonymous path but not a versionId" in {
  //  intercept[RuntimeException] { VersionedItemAdded(Workflow.of(Fail(None))) }
  //  intercept[RuntimeException] { VersionedItemAdded(Workflow(WorkflowPath("A") % "VERSION", Vector(Fail(None)))) }
  //}
  //
  //"VersionedItemChanged must have a non-anonymous path but not a versionId" in {
  //  intercept[RuntimeException] { VersionedItemChanged(Workflow.of(Fail(None))) }
  //  intercept[RuntimeException] { VersionedItemChanged(Workflow(WorkflowPath("A") % "VERSION", Vector(Fail(None)))) }
  //}

  "VersionedItemRemoved must have a non-anonymous path" in {
    intercept[RuntimeException] { VersionedItemRemoved(WorkflowPath.Anonymous) }
  }
}

object VersionedEventTest
{
  implicit private val itemPathJsonCodec: Codec[VersionedItemPath] =
    InventoryItemPath.jsonCodec(Set(WorkflowPath))

  implicit private val itemJsonCodec: TypedJsonCodec[VersionedItem] = TypedJsonCodec(
    Subtype(Workflow.jsonEncoder, Workflow.topJsonDecoder))

  private[VersionedEventTest] implicit val itemEventJsonCodec: TypedJsonCodec[VersionedEvent] =
    VersionedEvent.jsonCodec
}
