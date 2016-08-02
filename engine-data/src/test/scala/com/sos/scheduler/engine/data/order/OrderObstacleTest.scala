package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.order.OrderObstacle._
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class OrderObstacleTest extends FreeSpec {

  "JSON" - {
    addTest(Suspended,
      """{
        "type": "Suspended"
      }""")
    addTest(Blacklisted,
      """{
        "type": "Blacklisted"
      }""")
    addTest(Setback(Instant.parse("2016-08-01T11:22:33.444Z")),
      """{
        "type": "Setback",
        "at": "2016-08-01T11:22:33.444Z"
      }""")
  }

  private def addTest(obstacle: OrderObstacle, json: String): Unit = {
    s"$obstacle" in {
      val jsObject = json.parseJson.asJsObject
      assert(obstacle.toJson == jsObject)
      assert(jsObject.convertTo[OrderObstacle] == obstacle)
    }
  }
}
