package com.sos.scheduler.engine.data.order

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class OrderStatisticsTest extends FreeSpec {

  "plus" in {
    assert(
      OrderStatistics(   1,    2,    3,    4,    5,    6,    7,    8,    9,   10,   11,   12) +
      OrderStatistics(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 1000, 1100, 1200) ==
      OrderStatistics(1001, 2002, 3003, 4004, 5005, 6006, 7007, 8008, 9009, 1010, 1111, 1212))
  }

  "JSON" in {
    val o = OrderStatistics(
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
      fileOrder = 12)
    val json = """{
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
      }""".parseJson
    assert(o.toJson == json)
    assert(json.convertTo[OrderStatistics] == o)
  }
}
