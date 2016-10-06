package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, KeyedEvent}
import com.sos.scheduler.engine.data.events.schedulerKeyedEventJsonFormat
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class OrderStatisticsChangedTest extends FreeSpec {

  "JSON" in {
    val o = KeyedEvent(OrderStatisticsChanged(
      OrderStatistics(
        total = 1,
        notPlanned = 2,
        planned = 3,
        due = 4,
        running = 5,
        inTask = 6,
        inProcess = 7,
        setback = 8,
        suspended = 9,
        blacklisted = 10,
        permanent = 11,
        fileOrder = 12)))
    val json = """{
        "TYPE": "OrderStatisticsChanged",
        "orderStatistics": {
          "total": 1,
          "notPlanned": 2,
          "planned": 3,
          "due": 4,
          "running": 5,
          "inTask": 6,
          "inProcess": 7,
          "setback": 8,
          "suspended": 9,
          "permanent": 11,
          "blacklisted": 10,
          "fileOrder": 12
        }
      }""".parseJson
    assert(o.toJson == json)
    assert(json.convertTo[AnyKeyedEvent] == o)
  }
}
