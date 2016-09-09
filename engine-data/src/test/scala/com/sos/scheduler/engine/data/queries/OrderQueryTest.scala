package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.folder.FolderPath
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

  "OrderQuery.All JSON" in {
    check(OrderQuery.All, "{}")
  }

  "OrderQuery JSON" in {
    check(OrderQuery(
      jobChainPathQuery = PathQuery(FolderPath("/FOLDER")),
      orderId = Some(OrderId("ORDER-ID")),
      isDistributed = Some(true),
      isSuspended = Some(true),
      isSetback = Some(false),
      isBlacklisted = Some(false),
      isOrderSourceType = Some(Set(OrderSourceType.FileOrder)),
      isOrderProcessingState = Some(Set(NotPlanned.getClass, classOf[Setback])),
      notInTaskLimitPerNode = Some(1000)),
      """{
        "path": "/FOLDER/",
        "orderId": "ORDER-ID",
        "isDistributed": true,
        "isSuspended": true,
        "isSetback": false,
        "isBlacklisted": false,
        "isOrderSourceType": [ "FileOrder" ],
        "isOrderProcessingState": [ "NotPlanned", "Setback" ],
        "notInTaskLimitPerNode": 1000
      }""")
  }

  private def check(q: OrderQuery, json: String) = {
    assert(q.toJson == json.parseJson)
    assert(json.parseJson.convertTo[OrderQuery] == q)
  }
}

