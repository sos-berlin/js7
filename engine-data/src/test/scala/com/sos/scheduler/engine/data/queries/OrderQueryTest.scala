package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.folder.FolderPath
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.{JobChainPath, NodeId}
import com.sos.scheduler.engine.data.order.OrderProcessingState.{NotPlanned, Setback}
import com.sos.scheduler.engine.data.order.{OrderId, OrderProcessingState, OrderSourceType}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class OrderQueryTest extends FreeSpec {

  "matches" - {
    // See OnlyOrderQueryTest
  }

  "orderKeyOption" in {
    assert(OrderQuery().orderKeyOption == None)
    assert(OrderQuery(orderIds = Some(Set(OrderId("1")))).orderKeyOption == None)
    assert(OrderQuery(JobChainNodeQuery(JobChainQuery(PathQuery(JobChainPath("/A"))))).orderKeyOption == None)
    assert(OrderQuery(JobChainNodeQuery(JobChainQuery(PathQuery(JobChainPath("/A")))), orderIds = Some(Set(OrderId("1")))).orderKeyOption ==
      Some(JobChainPath("/A") orderKey "1"))
    assert(OrderQuery(JobChainNodeQuery(JobChainQuery(PathQuery(JobChainPath("/A")))), orderIds = Some(Set(OrderId("1"), OrderId("2")))).orderKeyOption == None)
  }

  "toPathAndParameters" in {
    check(
      OrderQuery(),
      "/" → Map())
    check(
      OrderQuery(JobChainNodeQuery(JobChainQuery(JobChainPath("/JOB_CHAIN")))),
      "/JOB_CHAIN" → Map())
    check(
      OrderQuery(JobChainNodeQuery(JobChainQuery(JobChainPath("/JOB_CHAIN"), isDistributed = Some(true)))),
      "/JOB_CHAIN" → Map("isDistributed" → "true"))
    check(
      OrderQuery(JobChainNodeQuery(
        JobChainQuery(PathQuery[JobChainPath]("/JOB_CHAIN"), isDistributed = Some(true)),
        jobPaths = Some(Set(JobPath("/A"), JobPath("/B"))),
        nodeIds = Some(Set(NodeId("100"), NodeId("200")))),
        orderIds = Some(Set(OrderId("1"), OrderId("2"))),
        isSuspended = Some(true),
        isSetback = Some(false),
        isBlacklisted = Some(true),
        isOrderSourceType = Some(Set(OrderSourceType.AdHoc, OrderSourceType.FileOrder)),
        isOrderProcessingState = Some(Set(OrderProcessingState.NotPlanned.getClass, classOf[OrderProcessingState.Planned])),
        orIsSuspended = true,
        notInTaskLimitPerNode = Some(1000)),
      "/JOB_CHAIN" → Map(
        "isDistributed" → "true",
        "jobPaths" → "/A,/B",
        "nodeIds" → "100,200",
        "orderIds" → "1,2",
        "isSuspended" → "true",
        "isSetback" → "false",
        "isBlacklisted" → "true",
        "isOrderSourceType" → "AdHoc,FileOrder",
        "isOrderProcessingState" → "NotPlanned,Planned",
        "orIsSuspended" → "true",
        "notInTaskLimitPerNode" → "1000"))

    def check(q: OrderQuery, pathAndParameters: (String, Map[String, String])): Unit = {
      assert(q.toPathAndParameters == pathAndParameters)
      assert(OrderQuery.pathAndParameterSerializable.fromPathAndParameters(pathAndParameters) == q)
    }
  }


  "JSON" - {
    "OrderQuery.All" in {
      check(OrderQuery.All, "{}")
    }

    "OrderQuery" in {
      check(OrderQuery(
        nodeQuery = JobChainNodeQuery(
          JobChainQuery(
            pathQuery = PathQuery(FolderPath("/FOLDER")),
            isDistributed = Some(true)),
          nodeIds = Some(Set(NodeId("100"), NodeId("200"))),
          jobPaths = Some(Set(JobPath("/A"), JobPath("/B")))),
        orderIds = Some(Set(OrderId("A-ORDER-ID"), OrderId("B-ORDER-ID"))),
        isSuspended = Some(true),
        isSetback = Some(false),
        isBlacklisted = Some(false),
        isOrderSourceType = Some(Set(OrderSourceType.FileOrder)),
        isOrderProcessingState = Some(Set(NotPlanned.getClass, classOf[Setback])),
        notInTaskLimitPerNode = Some(1000)),
        """{
          "path": "/FOLDER/",
          "isDistributed": true,
          "orderIds": [
            "A-ORDER-ID",
            "B-ORDER-ID"
          ],
          "nodeIds": [
            "100",
            "200"
          ],
          "jobPaths": [
            "/A",
            "/B"
          ],
          "isSuspended": true,
          "isSetback": false,
          "isBlacklisted": false,
          "isOrderSourceType": [ "FileOrder" ],
          "isOrderProcessingState": [ "NotPlanned", "Setback" ],
          "notInTaskLimitPerNode": 1000
        }""")
    }

    "pathQuery" - {
      "Single JobChainPath" in {
        check(
          OrderQuery(JobChainNodeQuery(JobChainQuery(pathQuery = PathQuery(JobChainPath("/FOLDER/JOBCHAIN"))))),
          """{
            "path": "/FOLDER/JOBCHAIN"
          }""")
      }

      "Folder, recursive" in {
        check(
          OrderQuery(JobChainNodeQuery(JobChainQuery(pathQuery = PathQuery(FolderPath("/FOLDER"), isRecursive = true)))),
          """{
            "path": "/FOLDER/"
          }""")
      }

      "Folder, not recursive" in {
        check(
          OrderQuery(JobChainNodeQuery(JobChainQuery(pathQuery = PathQuery(FolderPath("/FOLDER"), isRecursive = false)))),
          """{
            "path": "/FOLDER/*"
          }""")
      }
    }

    "OrderQuery.orIsSuspended" in {
      check(
        OrderQuery(orIsSuspended = true),
        """{
          "orIsSuspended": true
        }""")
    }

    def check(q: OrderQuery, json: String) = {
      assert(q.toJson == json.parseJson)
      assert(json.parseJson.convertTo[OrderQuery] == q)
    }
  }

  //"withoutPathToMap" in { Please see OrderQueryHttpTest }
}
