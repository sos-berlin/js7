package js7.data.item

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichJson}
import js7.base.crypt.SignedString
import js7.base.crypt.silly.SillySignature
import js7.data.controller.ControllerState._
import js7.data.item.ItemOperation.{AddVersion, SimpleAddOrChange, SimpleDelete, VersionedAddOrChange, VersionedDelete}
import js7.data.lock.{Lock, LockId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class ItemOperationTest extends AnyFreeSpec
{
  "JSON" - {
    "SimpleAddOrChange" in {
      testJson[ItemOperation](
        SimpleAddOrChange(Lock(LockId("LOCK"))),
        // itemRevision is optional and should not be given !!!
        json"""{
          "TYPE": "SimpleAddOrChange",
          "item": {
            "TYPE": "Lock",
            "id": "LOCK",
            "limit": 1
          }
        }""")
    }

    "SimpleDelete" in {
      testJson[ItemOperation](
        SimpleDelete(LockId("LOCK")),
        json"""{
          "TYPE": "SimpleDelete",
          "id": "Lock:LOCK"
        } """)
    }

    "AddVersion" in {
      testJson[ItemOperation](
        AddVersion(VersionId("1")),
        json"""{
          "TYPE": "AddVersion",
          "versionId": "1"
        }""")
    }

    "VersionedAddOrChange" in {
      testJson[ItemOperation](
        VersionedAddOrChange(
          SignedString(Workflow.of(WorkflowPath("WORKFLOW") ~ "1").asJson.compactPrint,
            SillySignature("XX").toGenericSignature)),
        json"""{
          "TYPE": "VersionedAddOrChange",
          "signedString": {
            "signature": {
              "TYPE": "Silly",
              "signatureString": "XX"
            },
            "string": "{\"path\":\"WORKFLOW\",\"versionId\":\"1\",\"instructions\":[]}"
          }
        } """)
    }

    "VersionedDelete" in {
      testJson[ItemOperation](
        VersionedDelete(WorkflowPath("WORKFLOW")),
        json"""{
          "TYPE": "VersionedDelete",
          "path": "Workflow:WORKFLOW"
        } """)
    }
  }
}
