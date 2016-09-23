package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.folder.FolderPath
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.order.OrderProcessingState.{NotPlanned, Setback}
import com.sos.scheduler.engine.data.order.{OrderId, OrderSourceType}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class OrderQueryTest extends FreeSpec {

  "orderKeyOption" in {
    assert(OrderQuery().orderKeyOption == None)
    assert(OrderQuery(orderIds = Some(Set(OrderId("1")))).orderKeyOption == None)
    assert(OrderQuery(JobChainQuery(PathQuery(JobChainPath("/A")))).orderKeyOption == None)
    assert(OrderQuery(JobChainQuery(PathQuery(JobChainPath("/A"))), orderIds = Some(Set(OrderId("1")))).orderKeyOption ==
      Some(JobChainPath("/A") orderKey "1"))
    assert(OrderQuery(JobChainQuery(PathQuery(JobChainPath("/A"))), orderIds = Some(Set(OrderId("1"), OrderId("2")))).orderKeyOption == None)
  }

  "JSON" - {
    "OrderQuery.All" in {
      check(OrderQuery.All, "{}")
    }

    "OrderQuery" in {
      check(OrderQuery(
        jobChainQuery = JobChainQuery(
          pathQuery = PathQuery(FolderPath("/FOLDER")),
          isDistributed = Some(true)),
        orderIds = Some(Set(OrderId("A-ORDER-ID"), OrderId("B-ORDER-ID"))),
        jobPaths = Some(Set(JobPath("/A"), JobPath("/B"))),
        isSuspended = Some(true),
        isSetback = Some(false),
        isBlacklisted = Some(false),
        isOrderSourceType = Some(Set(OrderSourceType.FileOrder)),
        isOrderProcessingState = Some(Set(NotPlanned.getClass, classOf[Setback])),
        notInTaskLimitPerNode = Some(1000)),
        """{
          "path": "/FOLDER/",
          "orderIds": [
            "A-ORDER-ID",
            "B-ORDER-ID"
          ],
          "jobPaths": [
            "/A",
            "/B"
          ],
          "isDistributed": true,
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
          OrderQuery(JobChainQuery(pathQuery = PathQuery(JobChainPath("/FOLDER/JOBCHAIN")))),
          """{
            "path": "/FOLDER/JOBCHAIN"
          }""")
      }

      "Folder, recursive" in {
        check(
          OrderQuery(JobChainQuery(pathQuery = PathQuery(FolderPath("/FOLDER"), isRecursive = true))),
          """{
            "path": "/FOLDER/"
          }""")
      }

      "Folder, not recursive" in {
        check(
          OrderQuery(JobChainQuery(pathQuery = PathQuery(FolderPath("/FOLDER"), isRecursive = false))),
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

  "commaSplittedAsSet" in {
    val asInts = OrderQuery.commaSplittedAsSet(_.toInt)
    assert(asInts("") == Set())
    assert(asInts("1") == Set(1))
    assert(asInts("1") == Set(1))
    assert(asInts("1") == Set(1))
    assert(asInts("1,22,333") == Set(1,22,333))
    assert(asInts("1,22,333") == Set(1,22,333))
    intercept[IllegalArgumentException] { asInts(" ") }
    intercept[IllegalArgumentException] { asInts(",") }
    intercept[IllegalArgumentException] { asInts(" 1") }
    intercept[IllegalArgumentException] { asInts("1 ") }
    intercept[IllegalArgumentException] { asInts("1,") }
    intercept[IllegalArgumentException] { asInts("1, 2") }
  }

  //"withoutPathToMap" in { Please see OrderQueryHttpTest }
}
