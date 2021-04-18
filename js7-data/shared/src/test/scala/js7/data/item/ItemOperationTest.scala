package js7.data.item

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.crypt.silly.SillySigner
import js7.data.controller.ControllerState
import js7.data.controller.ControllerState._
import js7.data.item.ItemOperation.{AddVersion, SignedAddOrChange, SimpleAddOrChange, SimpleDelete, VersionedDelete}
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

    "SignedAddOrChange" in {
      val itemSigner = new ItemSigner(SillySigner.Default, ControllerState.signableItemJsonCodec)
      val workflow = Workflow.of(WorkflowPath("WORKFLOW") ~ "1")
      testJson[ItemOperation](
        SignedAddOrChange(itemSigner.toSignedString(workflow)),
        json"""{
          "TYPE": "SignedAddOrChange",
          "signedString": {
            "signature": {
              "TYPE": "Silly",
              "signatureString": "SILLY-SIGNATURE"
            },
            "string": "{\"TYPE\":\"Workflow\",\"path\":\"WORKFLOW\",\"versionId\":\"1\",\"instructions\":[]}"
          }
        }""")
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
