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
        notSuspendedNotPlanned = 3,
        planned = 4,
        due = 5,
        started = 6,
        inTask = 7,
        inProcess = 8,
        setback = 9,
        waitingForResource = 10,
        notSuspendedWaitingForResource = 11,
        suspended = 12,
        blacklisted = 13,
        permanent = 14,
        fileOrder = 15)))
    val json = """{
        "TYPE": "OrderStatisticsChanged",
        "orderStatistics": {
          "total": 1,
          "notPlanned": 2,
          "notSuspendedNotPlanned": 3,
          "planned": 4,
          "due": 5,
          "started": 6,
          "inTask": 7,
          "inProcess": 8,
          "setback": 9,
          "waitingForResource": 10,
          "notSuspendedWaitingForResource": 11,
          "suspended": 12,
          "blacklisted": 13,
          "permanent": 14,
          "fileOrder": 15
        }
      }""".parseJson
    assert(o.toJson == json)
    assert(json.convertTo[AnyKeyedEvent] == o)
  }
}
