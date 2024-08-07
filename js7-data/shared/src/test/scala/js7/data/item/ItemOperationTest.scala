package js7.data.item

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.crypt.silly.SillySigner
import js7.base.test.OurTestSuite
import js7.data.controller.ControllerState
import js7.data.controller.ControllerState.*
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddOrChangeSimple, AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.lock.{Lock, LockPath}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.CirceJsonTester.testJson

final class ItemOperationTest extends OurTestSuite:

  "JSON" - {
    "AddOrChangeSimple" in:
      testJson[ItemOperation](
        AddOrChangeSimple(Lock(LockPath("LOCK"))),
        // itemRevision is optional and should not be given !!!
        json"""{
          "TYPE": "AddOrChangeSimple",
          "item": {
            "TYPE": "Lock",
            "path": "LOCK",
            "limit": 1
          }
        }""")

    "DeleteSimple" in:
      testJson[ItemOperation](
        DeleteSimple(LockPath("LOCK")),
        json"""{
          "TYPE": "DeleteSimple",
          "path": "Lock:LOCK"
        } """)

    "AddVersion" in:
      testJson[ItemOperation](
        AddVersion(VersionId("1")),
        json"""{
          "TYPE": "AddVersion",
          "versionId": "1"
        }""")

    "AddOrChangeSigned" in:
      val itemSigner = new ItemSigner(SillySigner.Default, ControllerState.signableItemJsonCodec)
      val workflow = Workflow.of(WorkflowPath("WORKFLOW") ~ "1")
      testJson[ItemOperation](
        AddOrChangeSigned(itemSigner.toSignedString(workflow)),
        json"""{
          "TYPE": "AddOrChangeSigned",
          "signedString": {
            "signature": {
              "TYPE": "Silly",
              "signatureString": "SILLY-SIGNATURE"
            },
            "string": "{\"TYPE\":\"Workflow\",\"path\":\"WORKFLOW\",\"versionId\":\"1\",\"instructions\":[]}"
          }
        }""")

    "RemoveVersioned" in:
      testJson[ItemOperation](
        RemoveVersioned(WorkflowPath("WORKFLOW")),
        json"""{
          "TYPE": "RemoveVersioned",
          "path": "Workflow:WORKFLOW"
        } """)
  }
