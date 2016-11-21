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
final class JocJocOrderStatisticsChangedTest extends FreeSpec {

  "JSON" in {
    val o = KeyedEvent(JocOrderStatisticsChanged(
      JocOrderStatistics(
        total = -1,
        notPlanned = 2,
        planned = 3,
        due = 4,
        started = 5,
        inTask = 6,
        inTaskProcess = 7,
        occupiedByClusterMember = 8,
        setback = 9,
        waitingForResource = 10,
        suspended = 11,
        blacklisted = 12,
        permanent = 13,
        fileOrder = 14)))
    val json = """{
        "TYPE": "JocOrderStatisticsChanged",
          "orderStatistics": {
          "total": -1,
          "notPlanned": 2,
          "planned": 3,
          "due": 4,
          "started": 5,
          "inTask": 6,
          "inTaskProcess": 7,
          "occupiedByClusterMember": 8,
          "setback": 9,
          "waitingForResource": 10,
          "suspended": 11,
          "blacklisted": 12,
          "permanent": 13,
          "fileOrder": 14
        }
      }""".parseJson
    assert(o.toJson == json)
    assert(json.convertTo[AnyKeyedEvent] == o)
  }
}
